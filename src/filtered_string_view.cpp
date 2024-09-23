#include "./filtered_string_view.h"

#include <algorithm>
#include <cstring>
#include <utility>

fsv::filtered_string_view::filtered_string_view(char const* str, std::size_t const& length, filter predicate) noexcept
: data_{str}
, length_{length}
, predicate_{std::move(predicate)} {}

fsv::filter fsv::filtered_string_view::default_predicate = [](char const&) noexcept -> bool { return true; };

fsv::filtered_string_view::filtered_string_view() noexcept
: filtered_string_view{nullptr, 0} {}

fsv::filtered_string_view::filtered_string_view(std::string const& str, fsv::filter predicate) noexcept
: filtered_string_view{str.data(), str.size(), std::move(predicate)} {}

fsv::filtered_string_view::filtered_string_view(char const* str, fsv::filter predicate) noexcept
: filtered_string_view{str, str ? std::strlen(str) : 0, std::move(predicate)} {}

fsv::filtered_string_view::filtered_string_view(filtered_string_view&& other) noexcept
: filtered_string_view{std::exchange(other.data_, nullptr),
                       std::exchange(other.length_, 0),
                       std::exchange(other.predicate_, default_predicate)} {}

auto fsv::filtered_string_view::operator=(filtered_string_view&& other) noexcept -> filtered_string_view& {
	if (this != &other) {
		data_ = std::exchange(other.data_, nullptr);
		length_ = std::exchange(other.length_, 0);
		predicate_ = std::exchange(other.predicate_, default_predicate);
	}
	return *this;
}

auto fsv::filtered_string_view::operator[](int const& n) const -> char const& {
	return *std::next(begin(), n);
}

fsv::filtered_string_view::operator std::string() const {
	auto filtered_string = std::string{};
	std::copy_if(data_, data_ + length_, std::back_inserter(filtered_string), predicate_);
	return filtered_string;
}

auto fsv::filtered_string_view::at(int const& index) const -> char const& {
	auto const result = find_filtered_char(index);
	if (not result) {
		throw std::domain_error{"filtered_string_view::at(" + std::to_string(index) + "): invalid index"};
	}
	return *result;
}

auto fsv::filtered_string_view::find_filtered_char(int const& index) const -> char const* {
	if (index < 0 or static_cast<std::size_t>(index) >= length_) {
		return nullptr;
	}

	auto filtered_index = 0;
	auto const result = std::find_if(data_, data_ + length_, [&filtered_index, this, index](char const& c) -> bool {
		return predicate_(c) && filtered_index++ == index;
	});

	return result != data_ + length_ ? result : nullptr;
}

auto fsv::filtered_string_view::size() const -> std::size_t {
	return static_cast<std::size_t>(std::ranges::count_if(data_, data_ + length_, predicate_));
}

auto fsv::filtered_string_view::empty() const -> bool {
	return size() == 0;
}

auto fsv::filtered_string_view::data() const noexcept -> char const* {
	return data_;
}

auto fsv::filtered_string_view::predicate() const noexcept -> filter const& {
	return predicate_;
}

auto fsv::operator==(filtered_string_view const& lhs, filtered_string_view const& rhs) -> bool {
	if (lhs.size() != rhs.size()) {
		return false;
	}
	return std::ranges::equal(lhs, rhs);
}

auto fsv::operator<=>(filtered_string_view const& lhs, filtered_string_view const& rhs) -> std::strong_ordering {
	return std::lexicographical_compare_three_way(
	    lhs.begin(),
	    lhs.end(),
	    rhs.begin(),
	    rhs.end(),
	    [](char const& a, char const& b) -> std::strong_ordering { return a <=> b; });
}

auto fsv::operator<<(std::ostream& os, filtered_string_view const& obj) -> std::ostream& {
	std::ranges::copy(obj, std::ostream_iterator<char>{os});
	return os;
}

fsv::filtered_string_view::iter::iter(int const& index, filtered_string_view const* fsv) noexcept
: index_{index}
, fsv_{fsv} {}

auto fsv::filtered_string_view::iter::operator*() const -> reference {
	return fsv_->at(index_);
}

auto fsv::filtered_string_view::iter::operator->() const noexcept -> pointer {}

auto fsv::filtered_string_view::iter::operator++() noexcept -> iter& {
	++index_;
	return *this;
}

auto fsv::filtered_string_view::iter::operator++(int) noexcept -> iter {
	auto const it = *this;
	++index_;
	return it;
}

auto fsv::filtered_string_view::iter::operator--() noexcept -> iter& {
	--index_;
	return *this;
}

auto fsv::filtered_string_view::iter::operator--(int) noexcept -> iter {
	auto const it = *this;
	--index_;
	return it;
}

auto fsv::operator==(fsv::filtered_string_view::iterator const& lhs,
                     fsv::filtered_string_view::iterator const& rhs) noexcept -> bool {
	return lhs.index_ == rhs.index_ and lhs.fsv_ == rhs.fsv_;
}

auto fsv::filtered_string_view::iter::get_pointer() const -> char const* {
	return fsv_->find_filtered_char(index_);
}

auto fsv::filtered_string_view::begin() const noexcept -> iterator {
	return iterator{0, this};
}

auto fsv::filtered_string_view::cbegin() const noexcept -> iterator {
	return const_iterator{0, this};
}

auto fsv::filtered_string_view::end() const -> iterator {
	return iterator{static_cast<int>(size()), this};
}

auto fsv::filtered_string_view::cend() const -> iterator {
	return const_iterator{static_cast<int>(size()), this};
}

auto fsv::filtered_string_view::rbegin() const -> reverse_iterator {
	return reverse_iterator{end()};
}

auto fsv::filtered_string_view::crbegin() const -> reverse_iterator {
	return const_reverse_iterator{end()};
}

auto fsv::filtered_string_view::rend() const noexcept -> reverse_iterator {
	return reverse_iterator{begin()};
}

auto fsv::filtered_string_view::crend() const noexcept -> reverse_iterator {
	return const_reverse_iterator{begin()};
}

auto fsv::compose(filtered_string_view const& fsv, std::vector<filter> const& filts) -> filtered_string_view {
	auto const combine_fileter = [&filts](auto const& c) -> bool {
		return std::ranges::all_of(filts, [&c](auto const& filter) { return filter(c); });
	};
	return filtered_string_view{fsv.data(), combine_fileter};
}

auto fsv::split(filtered_string_view const& fsv, filtered_string_view const& tok) -> std::vector<filtered_string_view> {
	if (tok.empty() or std::ranges::search(fsv, tok).empty()) {
		return std::vector<filtered_string_view>{fsv};
	}
	auto result = std::vector<filtered_string_view>{};
	auto begin = fsv.begin();

	while (begin != fsv.end()) {
		auto const& end = std::ranges::search(begin, fsv.end(), tok.begin(), tok.end());
		if (end.begin() == fsv.end()) {
			// Exit loop if the end of the string is reached
			break;
		}
		auto const& start_pos = begin.get_pointer() - fsv.data();
		auto const& end_pos = end.begin().get_pointer() - fsv.data();
		result.emplace_back(fsv.data(), fsv.make_range_predicate(start_pos, end_pos));

		// Update the beginning to the end of the last found delimiter
		begin = end.end();
	}

	// Add the last substring after the last delimiter to the end of the string.
	auto const& start_pos = std::prev(begin).get_pointer() - fsv.data() + 1;
	auto const& end_pos = static_cast<long>(fsv.length_);
	result.emplace_back(fsv.data(), fsv.make_range_predicate(start_pos, end_pos));

	return result;
}

auto fsv::substr(filtered_string_view const& fsv, int const& pos, int const& count) -> filtered_string_view {
	auto const& fsv_size = static_cast<int>(fsv.size());
	// Return an empty view if the starting position is beyond the available range
	if (pos >= fsv_size) {
		auto const& empty_pos = static_cast<long>(fsv.length_);
		return filtered_string_view{fsv.data(), fsv.make_range_predicate(empty_pos, empty_pos)};
	}

	// Calculate the real count of characters to take, spanning to the end if count is zero or negative
	auto const& rcount = count <= 0 ? fsv_size - pos : count;
	auto const& start_pos = &(fsv[pos]) - fsv.data();

	// Calculate the end position, ensuring it does not exceed the string length
	auto const& end_index = pos + rcount < fsv_size ? pos + rcount - 1 : fsv_size - 1;
	auto const& end_pos = &(fsv[end_index]) - fsv.data() + 1;

	return filtered_string_view{fsv.data(), fsv.make_range_predicate(start_pos, end_pos)};
}

auto fsv::filtered_string_view::make_range_predicate(long const& start_pos, long const& end_pos) const -> filter {
	return [start_pos, end_pos, this](char const& c) -> bool {
		// Determine the actual position of c in the original data
		auto const& index = &c - this->data_;
		// Check if index is within the bounds of the substring
		return index >= start_pos and index < end_pos and this->predicate_(c);
	};
}
