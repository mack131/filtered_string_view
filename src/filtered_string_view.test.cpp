#include "./filtered_string_view.h"

#include <catch2/catch.hpp>

#include <cstring>
#include <iostream>
#include <set>
#include <sstream>

TEST_CASE("test type of filter") {
	CHECK(typeid(fsv::filter) == typeid(std::function<bool(const char&)>));
}

TEST_CASE("2.3 spec tests") {
	SECTION("Check default predicate always return true") {
		auto const& c = GENERATE(take(256, range(std::numeric_limits<char>::min(), std::numeric_limits<char>::max())));
		CHECK(fsv::filtered_string_view::default_predicate(c));
	}
}

TEST_CASE("2.6 spec tests") {
	SECTION("at - not throw") {
		auto const& vowels = std::set<char>{'a', 'A', 'e', 'E', 'i', 'I', 'o', 'O', 'u', 'U'};
		auto const& is_vowel = [&vowels](char const& c) -> bool { return vowels.contains(c); };
		auto const& sv = fsv::filtered_string_view{"Malamute", is_vowel};
		CHECK(sv.at(0) == 'a');
	}

	SECTION("at - throw error") {
		auto const& sv = fsv::filtered_string_view{""};
		CHECK_THROWS_AS(sv.at(0), std::domain_error);
	}

	SECTION("size without predicate") {
		auto const& sv = fsv::filtered_string_view{"Maltese"};
		CHECK(sv.size() == 7);
	}

	SECTION("size with predicate") {
		auto const& pred = [](char const& c) -> bool { return c == 'o'; };
		auto const& sv = fsv::filtered_string_view{"Toy Poodle", pred};
		CHECK(sv.size() == 3);
	}

	SECTION("empty without predicate") {
		auto const& sv = fsv::filtered_string_view{"Australian Shephard"};
		auto const& empty_sv = fsv::filtered_string_view{};
		CHECK_FALSE(sv.empty());
		CHECK(empty_sv.empty());
	}

	SECTION("empty with predicate") {
		auto const& pred = [](char const& c) -> bool { return c == 'z'; };
		auto const& sv = fsv::filtered_string_view{"Border Collie", pred};
		CHECK(sv.empty());
	}

	SECTION("data - get the raw string") {
		auto const& s = "Sum 42";
		auto const& sv = fsv::filtered_string_view{s, [](char const&) -> bool { return false; }};

		auto const& buffer = std::stringstream{};
		auto const& oldCoutBuffer = std::cout.rdbuf();
		std::cout.rdbuf(buffer.rdbuf());

		auto ptr = sv.data();
		while (*ptr) {
			std::cout << *ptr++;
		}

		std::cout.rdbuf(oldCoutBuffer);
		auto const& output = std::string{buffer.str()};
		CHECK(output == s);
	}

	SECTION("predicate - check return predicate") {
		auto const& print_and_return_true = [](char const&) -> bool {
			std::cout << "hi!";
			return true;
		};
		auto const& s = fsv::filtered_string_view{"doggo", print_and_return_true};
		auto const& predicate = s.predicate();

		auto const& buffer = std::stringstream{};
		auto const& oldCoutBuffer = std::cout.rdbuf();
		std::cout.rdbuf(buffer.rdbuf());

		predicate(char{});

		std::cout.rdbuf(oldCoutBuffer);
		auto const& output = std::string{buffer.str()};
		CHECK(output == "hi!");
	}
}

TEST_CASE("Invalid case with at()", "[member]") {
	SECTION("Invalid with default constructor") {
		auto const& sv = fsv::filtered_string_view{};
		REQUIRE_THROWS_MATCHES(sv.at(0),
		                       std::domain_error,
		                       Catch::Matchers::Message("filtered_string_view::at(0): invalid index"));
	}

	SECTION("Out-of-bounds access") {
		auto const& pred = [](const char& c) -> bool { return c == '9' or c == '0' or c == ' '; };
		auto const& fsv = fsv::filtered_string_view{"only 90s kids understand", pred};
		REQUIRE_THROWS_MATCHES(fsv.at(10),
		                       std::domain_error,
		                       Catch::Matchers::Message("filtered_string_view::at(10): invalid index"));
	}

	SECTION("Empty string view") {
		auto const& pred = [](const char&) -> bool { return false; };
		auto const& fsv = fsv::filtered_string_view{"", pred};
		REQUIRE_THROWS_MATCHES(fsv.at(0),
		                       std::domain_error,
		                       Catch::Matchers::Message("filtered_string_view::at(0): invalid index"));
	}

	SECTION("All characters filtered out") {
		auto const& pred = [](const char&) -> bool { return false; };
		auto const& fsv = fsv::filtered_string_view{"non-empty", pred};
		REQUIRE_THROWS_MATCHES(fsv.at(0),
		                       std::domain_error,
		                       Catch::Matchers::Message("filtered_string_view::at(0): invalid index"));
	}
}

TEST_CASE("Test at() with string constructor", "[member]") {
	auto const& s = std::string{"Hello world!"};
	auto const& sv1 = fsv::filtered_string_view{s};
	SECTION("Invalid with string oversize and negative index") {
		REQUIRE_THROWS_MATCHES(sv1.at(-1),
		                       std::domain_error,
		                       Catch::Matchers::Message("filtered_string_view::at(-1): invalid index"));
		REQUIRE_THROWS_MATCHES(sv1.at(12),
		                       std::domain_error,
		                       Catch::Matchers::Message("filtered_string_view::at(12): invalid index"));
	}

	SECTION("Valid index in filtered string") {
		CHECK(sv1.at(0) == 'H');
		CHECK(sv1.at(5) == ' ');
		CHECK(sv1.at(11) == '!');
	}

	auto const& pred = [](char const& c) -> bool { return c >= 'a' and c <= 'z'; };
	auto const& sv2 = fsv::filtered_string_view{s, pred};
	SECTION("Invalid with null terminate and predicate oversize") {
		REQUIRE_THROWS_MATCHES(sv2.at(9),
		                       std::domain_error,
		                       Catch::Matchers::Message("filtered_string_view::at(9): invalid index"));
	}

	SECTION("Valid with predicate") {
		CHECK(sv2.at(0) == 'e');
		CHECK(sv2.at(4) == 'w');
		CHECK(sv2.at(8) == 'd');
	}
}

TEST_CASE("Test at() with null terminate constructor", "[member]") {
	auto const& sv1 = fsv::filtered_string_view{"Hello world!"};
	SECTION("Invalid with null terminate oversize and negative index") {
		REQUIRE_THROWS_MATCHES(sv1.at(-1),
		                       std::domain_error,
		                       Catch::Matchers::Message("filtered_string_view::at(-1): invalid index"));
		REQUIRE_THROWS_MATCHES(sv1.at(12),
		                       std::domain_error,
		                       Catch::Matchers::Message("filtered_string_view::at(12): invalid index"));
	}

	SECTION("Valid without predicate") {
		CHECK(sv1.at(0) == 'H');
		CHECK(sv1.at(5) == ' ');
		CHECK(sv1.at(11) == '!');
	}

	auto const& pred = [](char const& c) -> bool { return c >= 'a' and c <= 'z'; };
	auto const& sv2 = fsv::filtered_string_view{"Hello world!", pred};
	SECTION("Invalid with null terminate and predicate oversize") {
		REQUIRE_THROWS_MATCHES(sv2.at(9),
		                       std::domain_error,
		                       Catch::Matchers::Message("filtered_string_view::at(9): invalid index"));
	}

	SECTION("Valid with predicate") {
		CHECK(sv2.at(0) == 'e');
		CHECK(sv2.at(4) == 'w');
		CHECK(sv2.at(8) == 'd');
	}
}

TEST_CASE("Size Method of filtered_string_view", "[member]") {
	SECTION("No filter applied") {
		auto const& sv = fsv::filtered_string_view{"Maltese"};
		REQUIRE(sv.size() == 7);
	}

	SECTION("With filter excluding specific characters") {
		auto const& sv = fsv::filtered_string_view{"Toy Poodle", [](const char& c) -> bool { return c == 'o'; }};
		REQUIRE(sv.size() == 3);
	}

	SECTION("Empty string with no filter") {
		auto const& sv = fsv::filtered_string_view{""};
		REQUIRE(sv.size() == 0);
	}

	SECTION("Filter excludes all characters") {
		auto const& sv = fsv::filtered_string_view{"abcdef", [](const char&) -> bool { return false; }};
		REQUIRE(sv.size() == 0);
	}

	SECTION("Filter includes all characters") {
		auto const& sv = fsv::filtered_string_view{"abcdef", [](const char&) -> bool { return true; }};
		REQUIRE(sv.size() == 6);
	}

	SECTION("Same strings with different filters") {
		auto const& filter1 = [](char const&) -> bool { return true; };
		auto const& filter2 = [](char const&) -> bool { return false; };
		auto const& s1 = fsv::filtered_string_view{"data", filter1};
		auto const& s2 = fsv::filtered_string_view{"data", filter2};
		REQUIRE(s1.size() != s2.size());
	}

	SECTION("Different strings and predicates, same filtered size") {
		auto const filter1 = [](char const& c) -> bool { return c == 'a'; };
		auto const filter2 = [](char const& c) -> bool { return c == 'a' or c == 'b'; };
		auto const s1 = fsv::filtered_string_view{"aba", filter1};
		auto const s2 = fsv::filtered_string_view{"zada", filter2};

		REQUIRE(s1.size() == s2.size());
	}
}

TEST_CASE("Empty Method of filtered_string_view", "[member]") {
	SECTION("Non-empty string without filter") {
		auto const& sv = fsv::filtered_string_view{"Australian Shephard"};
		REQUIRE_FALSE(sv.empty());
	}

	SECTION("Empty string_view constructed with default constructor") {
		auto const& empty_sv = fsv::filtered_string_view{};
		REQUIRE(empty_sv.empty());
	}

	SECTION("All characters are filtered out") {
		auto const& sv = fsv::filtered_string_view{"Border Collie", [](const char& c) -> bool { return c == 'z'; }};
		REQUIRE(sv.empty());
	}

	SECTION("Filter includes all characters") {
		auto const& sv = fsv::filtered_string_view{"abcdef", [](const char&) -> bool { return true; }};
		REQUIRE_FALSE(sv.empty());
	}

	SECTION("Filter excludes all characters") {
		auto const& sv = fsv::filtered_string_view{"abcdef", [](const char&) -> bool { return false; }};
		REQUIRE(sv.empty());
	}

	SECTION("String with non-ASCII characters and filter removing all") {
		auto const& sv = fsv::filtered_string_view{"こんにちは世界", [](const char&) -> bool { return false; }};
		REQUIRE(sv.empty());
	}

	SECTION("String with non-ASCII characters and filter allowing all") {
		auto const& sv = fsv::filtered_string_view{"こんにちは世界", [](const char&) -> bool { return true; }};
		REQUIRE_FALSE(sv.empty());
	}
}

TEST_CASE("Data Method of filtered_string_view", "[member]") {
	SECTION("Returns pointer to the original string") {
		auto const& original = "Sum 42";
		auto const& sv = fsv::filtered_string_view{original};
		REQUIRE(sv.data() == original);
	}

	SECTION("Empty string_view constructed with default constructor") {
		auto const& empty_sv = fsv::filtered_string_view{};
		REQUIRE(empty_sv.data() == nullptr);
	}

	SECTION("Ignoring the filter") {
		auto const& pred = [](char const&) -> bool { return false; };
		auto const& original = "Sum 42";
		auto const& sv = fsv::filtered_string_view{original, pred};
		REQUIRE(sv.data() == original);
	}

	SECTION("Multiple instances pointing to different data") {
		auto const& str1 = "First string";
		auto const& str2 = "Second string";
		auto const& sv1 = fsv::filtered_string_view{str1};
		auto const& sv2 = fsv::filtered_string_view{str2};
		REQUIRE(sv1.data() == str1);
		REQUIRE(sv2.data() == str2);
	}

	SECTION("String constructor point to string.data") {
		auto const& str = std::string{"Hello!"};
		auto const& sv = fsv::filtered_string_view{str};
		REQUIRE(sv.data() == str.data());
	}
}

TEST_CASE("2.4 spec tests") {
	SECTION("test for default constructor") {
		auto const& sv = fsv::filtered_string_view{};
		CHECK(sv.empty());
	}

	SECTION("test implicit string constructor") {
		auto const& s = std::string{"cat"};
		auto const& sv = fsv::filtered_string_view{s};
		CHECK(sv.size() == 3);
	}

	SECTION("test string constructor with predicate") {
		auto const& s = std::string{"cat"};
		auto constexpr pred = [](char const& c) -> bool { return c == 'a'; };
		auto const sv = fsv::filtered_string_view{s, pred};
		CHECK(sv.size() == 1);
	}

	SECTION("test implicit null-terminated string constructor") {
		auto const& sv = fsv::filtered_string_view{"cat"};
		CHECK(sv.size() == 3);
	}

	SECTION("test null-terminated string with predicate constructor") {
		auto constexpr pred = [](const char& c) -> bool { return c == 'a'; };
		auto const& sv = fsv::filtered_string_view{"cat", pred};
		CHECK(sv.size() == 1);
	}

	SECTION("test copy and move constructor") {
		auto sv1 = fsv::filtered_string_view{"bulldog"};
		auto const copy = sv1;

		// pointers compare equal.
		CHECK(copy.data() == sv1.data());
		CHECK(copy.size() == sv1.size());

		auto const move = std::move(sv1);
		// true: sv1's guts were moved into `move`
		CHECK(sv1.data() == nullptr);
		CHECK(copy.size() == move.size());
		CHECK(sv1.empty());
	}
}

TEST_CASE("Default Constructor Test", "[constructor]") {
	auto const& sv = fsv::filtered_string_view{};
	REQUIRE(sv.empty());
	REQUIRE(sv.data() == nullptr);
}

TEST_CASE("Implicit String Constructor Test", "[constructor]") {
	SECTION("Normal string") {
		auto const& s = std::string{"hello"};
		auto const& sv = fsv::filtered_string_view{s};
		REQUIRE(sv.size() == 5);
	}
	SECTION("Empty string") {
		auto const& empty = std::string{};
		auto const& sv_empty = fsv::filtered_string_view{empty};
		REQUIRE(sv_empty.empty());
	}
}

TEST_CASE("String Constructor with Predicate Test", "[constructor]") {
	auto const& s = std::string{"example"};
	auto const& pred_all = [](auto const&) -> bool { return true; };
	auto const& pred_none = [](auto const&) -> bool { return false; };
	auto const& pred_no_e = [](auto const& c) -> bool { return c != 'e'; };

	SECTION("Predicate allows all") {
		auto const& sv_all = fsv::filtered_string_view{s, pred_all};
		REQUIRE(sv_all.size() == 7);
	}
	SECTION("Predicate allows none") {
		auto const& sv_none = fsv::filtered_string_view{s, pred_none};
		REQUIRE(sv_none.empty());
	}
	SECTION("Predicate excludes 'e'") {
		auto const& sv_no_e = fsv::filtered_string_view{s, pred_no_e};
		REQUIRE(sv_no_e.size() == 5);
	}
}

TEST_CASE("Implicit Null-Terminated String Constructor Test", "[constructor]") {
	SECTION("Basic C-style string") {
		auto const& sv = fsv::filtered_string_view{"hello"};
		REQUIRE(sv.size() == 5);
	}
	SECTION("Empty C-style string") {
		auto const& sv_empty = fsv::filtered_string_view{""};
		REQUIRE(sv_empty.empty());
	}
	SECTION("nullptr") {
		auto const& sv_empty = fsv::filtered_string_view{nullptr};
		REQUIRE(sv_empty.empty());
	}
}

TEST_CASE("Null-Terminated String with Predicate Test", "[constructor]") {
	auto constexpr text = "hello";
	auto const& pred_vowels = [](auto const& c) -> bool {
		return c == 'a' or c == 'e' or c == 'i' or c == 'o' or c == 'u';
	};

	auto const& sv = fsv::filtered_string_view{text, pred_vowels};
	// 'e' and 'o'
	REQUIRE(sv.size() == 2);
}

TEST_CASE("Copy and Move Constructors Test", "[constructor]") {
	auto original = fsv::filtered_string_view{"text"};
	auto const copy = fsv::filtered_string_view{original};
	REQUIRE(copy.data() == original.data());
	REQUIRE(copy.size() == original.size());

	auto const moved = std::move(original);
	REQUIRE(moved.data() != nullptr);
	// Verify moved-from state
	REQUIRE(original.data() == nullptr);
}

TEST_CASE("Check destructor inside class", "[constructor]") {
	REQUIRE(std::is_destructible<fsv::filtered_string_view>::value);
}

TEST_CASE("Invalid and Edge Cases", "[constructor]") {
	SECTION("Constructing from a nullptr") {
		auto constexpr null_str = nullptr;
		REQUIRE_NOTHROW(fsv::filtered_string_view{null_str});
	}

	SECTION("Check data with construct from a nullptr") {
		auto const& fsv = fsv::filtered_string_view{nullptr};
		REQUIRE(fsv.empty());
		REQUIRE(fsv.data() == nullptr);
	}
}

TEST_CASE("Copy Assignment Operator", "[operator]") {
	auto const& pred = [](char const& c) -> bool { return c == '4' or c == '2'; };
	auto fsv1 = fsv::filtered_string_view{"42 bro", pred};
	auto fsv2 = fsv::filtered_string_view{};

	SECTION("Normal copy assignment") {
		fsv2 = fsv1;
		REQUIRE(fsv1 == fsv2);
	}

	SECTION("Self-assignment") {
		auto const& fsv3 = fsv1;
		// Self assignment
		fsv1 = fsv3;
		// Check if unchanged
		REQUIRE(fsv1 == fsv3);
	}
}

TEST_CASE("Move Assignment Operator", "[operator]") {
	auto const& pred = [](char const& c) -> bool { return c == '8' or c == '9'; };
	auto fsv1 = fsv::filtered_string_view{"'89 baby", pred};
	auto fsv2 = fsv::filtered_string_view{};

	SECTION("Normal move assignment") {
		fsv2 = std::move(fsv1);
		REQUIRE(fsv2.size() == 2);
		REQUIRE(fsv1.empty());
		REQUIRE(fsv1.data() == nullptr);
	}

	SECTION("Self-move assignment") {
		auto& fsv_ref = fsv1;
		fsv1 = std::move(fsv_ref);
		// Should remain unchanged
		REQUIRE(fsv1.size() == 2);
		REQUIRE(fsv_ref.size() == 2);
		REQUIRE(fsv1 == fsv_ref);
	}
}

TEST_CASE("2.5 spec tests") {
	SECTION("Copy Assignment") {
		auto const& pred = [](const char& c) -> bool { return c == '4' or c == '2'; };
		auto const& fsv1 = fsv::filtered_string_view{"42 bro", pred};
		auto fsv2 = fsv::filtered_string_view{};
		fsv2 = fsv1;
		CHECK(fsv1 == fsv2);
	}

	SECTION("Move Assignment") {
		auto const& pred = [](const char& c) -> bool { return c == '8' or c == '9'; };
		auto fsv1 = fsv::filtered_string_view{"'89 baby", pred};
		auto fsv2 = fsv::filtered_string_view{};

		fsv2 = std::move(fsv1);

		CHECK(fsv1.empty());
		CHECK(fsv1.data() == nullptr);
		CHECK(not fsv2.empty());
	}
}

TEST_CASE("2.10 spec tests") {
	SECTION("ranges with vector") {
		auto const& s =
		    fsv::filtered_string_view{"puppy", [](char const& c) -> bool { return not(c == 'u' or c == 'y'); }};
		auto const& v = std::vector<char>{s.begin(), s.end()};

		auto const& buffer = std::stringstream{};
		auto const& oldCoutBuffer = std::cout.rdbuf();
		std::cout.rdbuf(buffer.rdbuf());

		std::cout << v[0] << v[1] << v[2];

		std::cout.rdbuf(oldCoutBuffer);
		auto const& output = std::string{buffer.str()};
		CHECK(output == "ppp");
	}

	SECTION("ranges from rbegin") {
		auto const& s =
		    fsv::filtered_string_view{"milo", [](char const& c) -> bool { return not(c == 'i' or c == 'o'); }};
		auto const& v = std::vector<char>{s.rbegin(), s.rend()};

		auto const& buffer = std::stringstream{};
		auto const& oldCoutBuffer = std::cout.rdbuf();
		std::cout.rdbuf(buffer.rdbuf());

		std::cout << v[0] << v[1];

		std::cout.rdbuf(oldCoutBuffer);
		auto const& output = std::string{buffer.str()};
		CHECK(output == "lm");
	}
}

TEST_CASE("2.9 spec tests") {
	SECTION("iterator with default predicate") {
		auto const& print_via_iterator = [](fsv::filtered_string_view const& sv) {
			std::ranges::copy(sv, std::ostream_iterator<char>(std::cout, " "));
		};
		auto const& fsv1 = fsv::filtered_string_view{"corgi"};

		auto const& buffer = std::stringstream{};
		auto const& oldCoutBuffer = std::cout.rdbuf();
		std::cout.rdbuf(buffer.rdbuf());

		print_via_iterator(fsv1);

		std::cout.rdbuf(oldCoutBuffer);
		auto const& output = std::string{buffer.str()};
		CHECK(output == "c o r g i ");
	}

	SECTION("iterator with predicate which removes lowercase vowels:") {
		auto const& fsv =
		    fsv::filtered_string_view{"samoyed", [](char const& c) -> bool {
			                              return not(c == 'a' or c == 'e' or c == 'i' or c == 'o' or c == 'u');
		                              }};
		auto const& it = fsv.begin();

		auto const& buffer = std::stringstream{};
		auto const& oldCoutBuffer = std::cout.rdbuf();
		std::cout.rdbuf(buffer.rdbuf());

		std::cout << *it << *std::next(it) << *std::next(it, 2) << *std::next(it, 3);

		std::cout.rdbuf(oldCoutBuffer);
		auto const& output = std::string{buffer.str()};
		CHECK(output == "smyd");
	}

	SECTION("iterator from end") {
		auto const& str = std::string("tosa");
		auto const& s = fsv::filtered_string_view{str};
		auto const& it = s.cend();

		auto const& buffer = std::stringstream{};
		auto const& oldCoutBuffer = std::cout.rdbuf();
		std::cout.rdbuf(buffer.rdbuf());

		std::cout << *std::prev(it) << *std::prev(it, 2);

		std::cout.rdbuf(oldCoutBuffer);
		auto const& output = std::string{buffer.str()};
		CHECK(output == "as");
	}
}

TEST_CASE("filtered_string_view iterator functionality", "[iterator]") {
	using Iterator = fsv::filtered_string_view::iterator;
	SECTION("Type traits") {
		STATIC_REQUIRE(std::is_same_v<Iterator::value_type, char>);
		STATIC_REQUIRE(std::is_same_v<Iterator::reference, const char&>);
		STATIC_REQUIRE(std::is_same_v<Iterator::pointer, void>);
		STATIC_REQUIRE(std::is_same_v<std::bidirectional_iterator_tag, std::iterator_traits<Iterator>::iterator_category>);
		STATIC_REQUIRE(std::is_same_v<Iterator::difference_type, std::ptrdiff_t>);
	}

	SECTION("Default constructor test") {
		auto const& it = fsv::filtered_string_view::iterator{};
		REQUIRE(it == it);
	}

	SECTION("Iterator operator test") {
		auto const& fsv = fsv::filtered_string_view{"abc"};
		auto const& it = fsv.begin();
		REQUIRE(*it == 'a');
		REQUIRE_NOTHROW(it.operator*());
		REQUIRE(it.operator*() == 'a');
		REQUIRE_NOTHROW(it.operator->());
	}

	SECTION("Iteration over string") {
		auto const& fsv = fsv::filtered_string_view{"hello"};
		auto collected = std::string{};
		std::ranges::copy(fsv, std::back_inserter(collected));
		REQUIRE(collected == "hello");
	}

	SECTION("Boundary condition: Begin and End") {
		auto const& fsv = fsv::filtered_string_view{"test"};
		auto it = fsv.begin();
		REQUIRE(*it == 't');
		it = fsv.end();
		REQUIRE_THROWS_AS(*it, std::domain_error);
	}

	SECTION("Filtering and iteration") {
		auto const& is_vowel = [](const char& c) -> bool {
			return c == 'a' or c == 'e' or c == 'i' or c == 'o' or c == 'u';
		};
		auto const& fsv = fsv::filtered_string_view{"banana", is_vowel};
		auto collected = std::string{};
		std::ranges::copy(fsv, std::back_inserter(collected));
		REQUIRE(collected == "aaa");
	}

	SECTION("Reverse iteration") {
		auto const& fsv = fsv::filtered_string_view{"abcde"};
		auto collected = std::string{};
		std::copy(std::make_reverse_iterator(fsv.end()),
		          std::make_reverse_iterator(fsv.begin()),
		          std::back_inserter(collected));
		REQUIRE(collected == "edcba");
	}

	SECTION("Same addres with get pointer on the begin of iterator and data") {
		auto const& fsv = fsv::filtered_string_view{"aeiou"};
		auto const it = fsv.begin();
		REQUIRE(it.get_pointer() == fsv.data());
	}
}

TEST_CASE("filtered_string_view iterator operator", "[iterator]") {
	auto const& fsv = fsv::filtered_string_view{"hello"};

	SECTION("Boundary conditions") {
		// First element
		REQUIRE(*(fsv.begin()) == 'h');
		// Last element, using std::prev to get to the last valid iterator position
		REQUIRE(*(std::prev(fsv.end())) == 'o');
	}

	SECTION("Equality and inequality") {
		REQUIRE(fsv.begin() == fsv.begin());
		REQUIRE(fsv.begin() != fsv.end());
	}

	SECTION("Increment and decrement") {
		auto it = fsv.begin();
		++it;
		REQUIRE(*it == 'e');
		--it;
		REQUIRE(*it == 'h');
	}
}

TEST_CASE("Post-increment and post-decrement", "[iterator]") {
	auto const& fsv = fsv::filtered_string_view{"hello"};
	auto it = fsv.begin();

	SECTION("Post-increment") {
		auto const post_inc = it++;
		REQUIRE(*post_inc == 'h');
		REQUIRE(*it == 'e');
	}

	SECTION("Post-decrement") {
		++it;
		auto const post_dec = it--;
		REQUIRE(*post_dec == 'e');
		REQUIRE(*it == 'h');
	}
}

TEST_CASE("2.5 spec rest, using iterator") {
	SECTION("Subscript") {
		auto const& pred = [](char const& c) -> bool { return c == '9' or c == '0' or c == ' '; };
		auto const& fsv1 = fsv::filtered_string_view{"only 90s kids understand", pred};
		CHECK(fsv1[2] == '0');
	}

	SECTION("String Type Conversion") {
		auto const& sv = fsv::filtered_string_view("vizsla");
		auto const& s = static_cast<std::string>(sv);
		CHECK_FALSE(sv.data() == s.data());
	}
}

TEST_CASE("Subscript Operator for filtered_string_view", "[subscript]") {
	SECTION("Valid index access") {
		auto const& pred = [](const char& c) -> bool { return c == '9' or c == '0' or c == ' '; };
		auto const& fsv = fsv::filtered_string_view{"only 90s kids understand", pred};
		REQUIRE(fsv[2] == '0');
	}

	SECTION("Boundary value test") {
		auto const& pred = [](const char& c) -> bool { return c == 'n'; };
		auto const& fsv = fsv::filtered_string_view{"non-empty", pred};
		REQUIRE(fsv[0] == 'n');
		REQUIRE(fsv[1] == 'n');
	}
}

TEST_CASE("Explicit Conversion to std::string", "[operator]") {
	SECTION("Conversion checks") {
		auto const& fsv = fsv::filtered_string_view{"vizsla"};
		auto const& s = static_cast<std::string>(fsv);
		REQUIRE(s.size() == 6);
		// They must not share the same data buffer
		REQUIRE(fsv.data() != s.data());
	}

	SECTION("Empty string_view conversion") {
		auto const& fsv = fsv::filtered_string_view{};
		auto const& result = static_cast<std::string>(fsv);
		REQUIRE(result.empty());
	}

	SECTION("All characters filtered out") {
		auto const& pred = [](char const&) -> bool { return false; };
		auto const& fsv = fsv::filtered_string_view{"abc", pred};
		auto const& result = static_cast<std::string>(fsv);
		REQUIRE(result.empty());
	}

	SECTION("String only containing filtered characters") {
		auto const& pred = [](char const& c) -> bool { return c != ' '; };
		auto const& fsv = fsv::filtered_string_view{"     ", pred};
		auto const& result = static_cast<std::string>(fsv);
		REQUIRE(result.empty());
	}

	SECTION("Handling non-ASCII characters") {
		auto const& pred = [](char const&) -> bool { return true; };
		auto const& fsv = fsv::filtered_string_view{"こんにちは世界", pred};
		auto const& result = static_cast<std::string>(fsv);
		REQUIRE(result == "こんにちは世界");
	}

	SECTION("Large data conversion") {
		auto const& large_string = std::string(10000, 'x');
		auto const& pred = [](char const&) -> bool { return true; };
		auto const& fsv = fsv::filtered_string_view{large_string, pred};
		auto const& result = static_cast<std::string>(fsv);
		REQUIRE(result == large_string);
	}
}

TEST_CASE("Predicate Method of filtered_string_view", "[member]") {
	SECTION("Accessing the predicate used for filtering") {
		auto const& print_and_return_true = [](char const&) -> bool {
			std::cout << "hi!";
			return true;
		};
		auto const& s = fsv::filtered_string_view{"doggo", print_and_return_true};

		auto const& predicate = s.predicate();

		auto const& buffer = std::stringstream{};
		auto const& oldCoutBuffer = std::cout.rdbuf();
		std::cout.rdbuf(buffer.rdbuf());

		REQUIRE(predicate('a'));

		std::cout.rdbuf(oldCoutBuffer);
		auto const& output = std::string{buffer.str()};
		REQUIRE(output == "hi!");
	}

	SECTION("Predicate functionality verification") {
		auto const return_false = [](char const&) -> bool { return false; };
		auto const& s = fsv::filtered_string_view{"hello", return_false};

		auto const& predicate = s.predicate();
		REQUIRE_FALSE(predicate('h'));
	}

	SECTION("Default constructed view predicate access") {
		auto const& s = fsv::filtered_string_view{};
		auto const& predicate = s.predicate();
		REQUIRE(predicate('a'));
	}

	SECTION("Multiple instances with different predicates") {
		auto const& pred1 = [](char const&) -> bool { return true; };
		auto const& pred2 = [](char const&) -> bool { return false; };
		auto const& s1 = fsv::filtered_string_view{"text1", pred1};
		auto const& s2 = fsv::filtered_string_view{"text2", pred2};

		auto const& predicate1 = s1.predicate();
		auto const& predicate2 = s2.predicate();
		REQUIRE(predicate1('a') == true);
		REQUIRE(predicate2('a') == false);
	}
}

TEST_CASE("2.7") {
	SECTION("Equality Comparison") {
		auto const& lo = fsv::filtered_string_view{"aaa"};
		auto const& hi = fsv::filtered_string_view{"zzz"};
		CHECK_FALSE(lo == hi);
		CHECK(lo != hi);
	}

	SECTION("Relational Comparison") {
		auto const& lo = fsv::filtered_string_view{"aaa"};
		auto const& hi = fsv::filtered_string_view{"zzz"};
		CHECK(lo < hi);
		CHECK(lo <= hi);
		CHECK_FALSE(lo > hi);
		CHECK_FALSE(lo >= hi);
		CHECK(lo <=> hi == std::strong_ordering::less);
	}

	SECTION("Output Stream") {
		auto const& fsv =
		    fsv::filtered_string_view{"c++ > rust > java", [](char const& c) -> bool { return c == 'c' or c == '+'; }};
		auto const& buffer = std::stringstream{};
		auto const& oldCoutBuffer = std::cout.rdbuf();
		std::cout.rdbuf(buffer.rdbuf());

		std::cout << fsv;

		std::cout.rdbuf(oldCoutBuffer);
		auto const& output = std::string{buffer.str()};
		CHECK(output == "c++");
	}
}

TEST_CASE("Equality and Inequality Operators for filtered_string_view", "[operators]") {
	SECTION("Same strings") {
		auto const& s1 = fsv::filtered_string_view{"hello"};
		auto const& s2 = fsv::filtered_string_view{"hello"};
		REQUIRE(s1 == s2);
		REQUIRE_FALSE(s1 != s2);
	}

	SECTION("Different strings") {
		auto const& s1 = fsv::filtered_string_view{"hello"};
		auto const& s2 = fsv::filtered_string_view{"world"};
		REQUIRE_FALSE(s1 == s2);
		REQUIRE(s1 != s2);
	}

	SECTION("Comparing empty strings") {
		auto const& s1 = fsv::filtered_string_view{""};
		auto const& s2 = fsv::filtered_string_view{""};
		REQUIRE(s1 == s2);
		REQUIRE_FALSE(s1 != s2);
	}

	SECTION("Strings of different lengths") {
		auto const& s1 = fsv::filtered_string_view{"hello"};
		auto const& s2 = fsv::filtered_string_view{"hello world"};
		REQUIRE_FALSE(s1 == s2);
		REQUIRE(s1 != s2);
	}

	SECTION("Same strings with different filters") {
		auto const& filter1 = [](char const&) -> bool { return true; };
		auto const& filter2 = [](char const&) -> bool { return false; };
		auto const& s1 = fsv::filtered_string_view{"data", filter1};
		auto const& s2 = fsv::filtered_string_view{"data", filter2};
		REQUIRE(s1 != s2);
		REQUIRE_FALSE(s1 == s2);
	}

	SECTION("Different strings and predicates, same filtered result") {
		auto const& filter1 = [](char const& c) -> bool { return c == 'a'; };
		auto const& filter2 = [](char const& c) -> bool { return c == 'a' or c == 'b'; };
		auto const& s1 = fsv::filtered_string_view{"aba", filter1};
		auto const& s2 = fsv::filtered_string_view{"zada", filter2};

		REQUIRE(s1 == s2);
		REQUIRE_FALSE(s1 != s2);
	}
}

TEST_CASE("Spaceship Operator for filtered_string_view", "[comparison]") {
	SECTION("Comparing the same strings") {
		auto const& s = fsv::filtered_string_view{"hello"};
		REQUIRE((s <=> s) == std::strong_ordering::equivalent);
		REQUIRE(s == s);
	}

	SECTION("Comparing lexicographically lesser to greater string") {
		auto const& lo = fsv::filtered_string_view{"aaa"};
		auto const& hi = fsv::filtered_string_view{"zzz"};
		REQUIRE((lo <=> hi) == std::strong_ordering::less);
		REQUIRE(lo < hi);
		REQUIRE((hi <=> lo) == std::strong_ordering::greater);
		REQUIRE(hi > lo);
	}

	SECTION("Comparing empty string to non-empty string") {
		auto const& empty = fsv::filtered_string_view{""};
		auto const& non_empty = fsv::filtered_string_view{"non-empty"};
		REQUIRE((empty <=> non_empty) == std::strong_ordering::less);
		REQUIRE(empty < non_empty);
		REQUIRE((non_empty <=> empty) == std::strong_ordering::greater);
		REQUIRE(non_empty > empty);
	}

	SECTION("Comparing two empty strings") {
		auto const& empty1 = fsv::filtered_string_view{""};
		auto const& empty2 = fsv::filtered_string_view{""};
		REQUIRE((empty1 <=> empty2) == std::strong_ordering::equivalent);
	}

	SECTION("Different strings with same filtered results") {
		auto const& filter = [](char const&) -> bool { return true; };
		auto const& s1 = fsv::filtered_string_view{"abc", filter};
		auto const& s2 = fsv::filtered_string_view{"abc", filter};
		REQUIRE((s1 <=> s2) == std::strong_ordering::equivalent);
	}
}

TEST_CASE("Spaceship Operator with filter effects for filtered_string_view", "[comparison]") {
	SECTION("Same strings with different filter results") {
		auto const& filter1 = [](char const& c) -> bool { return c == 'a'; };
		auto const& filter2 = [](char const& c) -> bool { return c == 'b'; };
		auto const& s1 = fsv::filtered_string_view{"aaaa", filter1};
		auto const& s2 = fsv::filtered_string_view{"aaaa", filter2};
		REQUIRE((s1 <=> s2) != std::strong_ordering::equivalent);
	}

	SECTION("Different strings with the same filter results") {
		auto const& filter = [](char const& c) -> bool { return c == 'a' or c == 'c'; };
		auto const& s1 = fsv::filtered_string_view{"aabbcc", filter};
		auto const& s2 = fsv::filtered_string_view{"bbaacc", filter};
		REQUIRE((s1 <=> s2) == std::strong_ordering::equivalent);
	}

	SECTION("Filters result in empty strings") {
		auto const& filter = [](char const&) -> bool { return false; };
		auto const& s1 = fsv::filtered_string_view{"any content here", filter};
		auto const& s2 = fsv::filtered_string_view{"different content here", filter};
		REQUIRE((s1 <=> s2) == std::strong_ordering::equivalent);
	}
}

TEST_CASE("Output Stream Operator for filtered_string_view", "[ostream]") {
	SECTION("Basic output functionality") {
		auto oss = std::ostringstream{};
		auto const& filter = [](char const& c) -> bool { return c == 'c' or c == '+'; };
		auto const& fsv = fsv::filtered_string_view{"c++ > rust > java", filter};
		oss << fsv;
		REQUIRE(oss.str() == "c++");
	}

	SECTION("Empty filtered_string_view output") {
		auto oss = std::ostringstream{};
		auto const& fsv = fsv::filtered_string_view{};
		oss << fsv;
		REQUIRE(oss.str().empty());
	}

	SECTION("Output with various character types") {
		auto oss = std::ostringstream{};
		auto const& filter = [](char const& c) -> bool { return not std::isspace(c); };
		auto const& fsv = fsv::filtered_string_view{"Hello, World!\n", filter};
		oss << fsv;
		REQUIRE(oss.str() == "Hello,World!");
	}

	SECTION("Continuous output on the same stream") {
		auto oss = std::ostringstream{};
		auto const& fsv1 = fsv::filtered_string_view{"First", [](char const&) -> bool { return true; }};
		auto const& fsv2 = fsv::filtered_string_view{"Second", [](char const&) -> bool { return true; }};
		oss << fsv1 << " " << fsv2;
		REQUIRE(oss.str() == "First Second");
	}

	SECTION("Continuous output on the same stream with false predicate") {
		auto oss = std::ostringstream{};
		auto const& fsv1 = fsv::filtered_string_view{"First", [](char const&) -> bool { return true; }};
		auto const& fsv2 = fsv::filtered_string_view{"Second", [](char const&) -> bool { return false; }};
		oss << fsv1 << " " << fsv2;
		REQUIRE(oss.str() == "First ");
	}

	SECTION("Handling non-ASCII characters") {
		auto oss = std::ostringstream{};
		auto const& filter = [](char const&) -> bool { return true; };
		auto const& fsv = fsv::filtered_string_view{"こんにちは", filter};
		oss << fsv;
		REQUIRE(oss.str() == "こんにちは");
	}
}

TEST_CASE("2.8") {
	SECTION("compose") {
		auto const& best_languages = fsv::filtered_string_view{"c / c++"};
		auto const& vf = std::vector<fsv::filter>{[](char const& c) -> bool { return c == 'c' or c == '+' or c == '/'; },
		                                          [](char const& c) -> bool { return c > ' '; },
		                                          [](char const&) -> bool { return true; }};

		auto const& sv = fsv::compose(best_languages, vf);

		auto const& buffer = std::stringstream{};
		auto oldCoutBuffer = std::cout.rdbuf();
		std::cout.rdbuf(buffer.rdbuf());

		std::cout << sv;

		std::cout.rdbuf(oldCoutBuffer);
		auto const& output = std::string{buffer.str()};
		CHECK(output == "c/c++");
	}

	SECTION("split - test 1") {
		auto const& interest = std::set<char>{'a', 'A', 'b', 'B', 'c', 'C', 'd', 'D', 'e', 'E', 'f', 'F', ' ', '/'};
		auto const& sv = fsv::filtered_string_view{"0xDEADBEEF / 0xdeadbeef",
		                                           [&interest](const char& c) -> bool { return interest.contains(c); }};
		auto const& tok = fsv::filtered_string_view{" / "};
		auto const& v = fsv::split(sv, tok);
		CHECK(v[0] == "DEADBEEF");
		CHECK(v[1] == "deadbeef");
	}

	SECTION("split - test 2") {
		auto const& fsv = fsv::filtered_string_view{"xax"};
		auto const& tok = fsv::filtered_string_view{"x"};
		auto const& v = fsv::split(fsv, tok);
		auto const& expected = std::vector<fsv::filtered_string_view>{"", "a", ""};

		CHECK(v == expected);
	}

	SECTION("split - test 3") {
		auto const& sv = fsv::filtered_string_view{"xx"};
		auto const& tok = fsv::filtered_string_view{"x"};
		auto const& v = fsv::split(sv, tok);
		auto const& expected = std::vector<fsv::filtered_string_view>{"", "", ""};

		CHECK(v == expected);
	}

	SECTION("substr - test 1") {
		auto const& sv = fsv::filtered_string_view{"Siberian Husky"};

		auto const& buffer = std::stringstream{};
		auto const& oldCoutBuffer = std::cout.rdbuf();
		std::cout.rdbuf(buffer.rdbuf());

		std::cout << fsv::substr(sv, 9);

		std::cout.rdbuf(oldCoutBuffer);
		auto const& output = std::string{buffer.str()};
		CHECK(output == "Husky");
	}

	SECTION("substr - test 2") {
		auto const& is_upper = [](char const& c) -> bool { return std::isupper(static_cast<unsigned char>(c)); };
		auto const& sv = fsv::filtered_string_view{"Sled Dog", is_upper};

		auto const& buffer = std::stringstream{};
		auto const& oldCoutBuffer = std::cout.rdbuf();
		std::cout.rdbuf(buffer.rdbuf());

		std::cout << fsv::substr(sv, 0, 2);

		std::cout.rdbuf(oldCoutBuffer);
		auto const& output = std::string{buffer.str()};
		CHECK(output == "SD");
	}
}

TEST_CASE("compose function for filtered_string_view", "[compose]") {
	SECTION("All filters pass") {
		auto const& fsv = fsv::filtered_string_view{"Example text"};
		auto const& filters = std::vector<fsv::filter>{[](char const&) -> bool { return true; }};
		auto const& composed = fsv::compose(fsv, filters);
		REQUIRE(composed.size() == fsv.size());
		REQUIRE(composed.data() == fsv.data());
	}

	SECTION("Short-circuit with a failing filter") {
		auto const& fsv = fsv::filtered_string_view{"Example text"};
		auto const& filters = std::vector<fsv::filter>{[](char const& c) -> bool { return c != 'E'; },
		                                               [](char const&) -> bool { return false; }};
		auto const composed = fsv::compose(fsv, filters);
		REQUIRE(composed.empty());
		REQUIRE(composed.data() == fsv.data());
	}

	SECTION("Empty filtered_string_view input") {
		auto const& fsv = fsv::filtered_string_view{""};
		auto const& filters = std::vector<fsv::filter>{[](char const&) -> bool { return true; }};
		auto const& composed = fsv::compose(fsv, filters);
		REQUIRE(composed.empty());
	}

	SECTION("Empty filter vector") {
		auto const& fsv = fsv::filtered_string_view{"Non-empty"};
		auto constexpr filters = std::vector<fsv::filter>{};
		auto const& composed = fsv::compose(fsv, filters);
		REQUIRE(composed.size() == fsv.size());
	}

	SECTION("All filters fail") {
		auto const& fsv = fsv::filtered_string_view{"abcdef"};
		auto const& filters = std::vector<fsv::filter>{[](char const&) -> bool { return false; }};
		auto const& composed = fsv::compose(fsv, filters);
		REQUIRE(composed.empty());
	}

	SECTION("Handling non-ASCII characters") {
		auto const& fsv = fsv::filtered_string_view{"こんにちは"};
		auto const& filters = std::vector<fsv::filter>{[](char const&) -> bool { return true; }};
		auto const& composed = fsv::compose(fsv, filters);
		REQUIRE(composed.size() == std::strlen("こんにちは"));
		REQUIRE(composed.data() == fsv.data());
	}

	SECTION("Short-circuit behavior verification") {
		auto const& fsv = fsv::filtered_string_view{"abcde", [](char const& c) -> bool { return c != 'a'; }};
		auto const& filters = std::vector<fsv::filter>{[](char const&) -> bool { return true; },
		                                               [](char const&) -> bool { return false; }};
		auto const& composed = fsv::compose(fsv, filters);
		REQUIRE(composed.empty());
	}

	SECTION("Underlying string are equal") {
		auto const& fsv = fsv::filtered_string_view{"Hello World!"};
		auto const& filters = std::vector<fsv::filter>{
		    [](char const& c) -> bool { return c == 'H' or c == 'W'; },
		    [](char const& c) -> bool { return c == 'a' or c == 'e' or c == 'i' or c == 'o' or c == 'u'; }};
		auto const& composed = fsv::compose(fsv, filters);
		REQUIRE(fsv.data() == composed.data());
	}
}

TEST_CASE("compose function with original predicate considered", "[compose]") {
	SECTION("Combining filters including the original") {
		auto const& fsv = fsv::filtered_string_view{"abcde", [](char const& c) -> bool { return c != 'a'; }};
		auto const& filters = std::vector<fsv::filter>{[](char const& c) -> bool { return c != 'b'; },
		                                               [](char const& c) -> bool { return c != 'c'; }};
		auto const& composed = fsv::compose(fsv, filters);
		// Only 'a', 'd' and 'e' should pass all filters
		REQUIRE(composed.size() == 3);
		REQUIRE(composed.data() == fsv.data());
	}

	SECTION("Original predicate fails") {
		auto const& fsv = fsv::filtered_string_view{"abcde", [](char const& c) -> bool { return c == 'a'; }};
		auto const& filters = std::vector<fsv::filter>{[](char const&) -> bool { return true; }};
		auto const& composed = fsv::compose(fsv, filters);
		// Pass all with the new predicate
		REQUIRE(composed.size() == 5);
		REQUIRE(composed == "abcde");
		REQUIRE(composed.data() == fsv.data());
	}

	SECTION("All predicates pass") {
		auto const& fsv = fsv::filtered_string_view{"abc", [](char const&) -> bool { return true; }};
		auto const& filters = std::vector<fsv::filter>{[](char const&) -> bool { return true; }};
		auto const& composed = fsv::compose(fsv, filters);
		// All characters 'abc' should pass
		REQUIRE(composed.size() == 3);
	}

	SECTION("Combining filters with original predicate considered first") {
		// Excludes 'a'
		auto const& fsv = fsv::filtered_string_view{"abcde", [](char const& c) -> bool { return c != 'a'; }};
		auto const& filters = std::vector<fsv::filter>{// Excludes 'b'
		                                               [](char const& c) -> bool { return c != 'b'; },
		                                               // Excludes 'c'
		                                               [](char const& c) -> bool { return c != 'c'; }};
		auto const& composed = fsv::compose(fsv, filters);
		// Only 'b' and 'c' should filtered
		REQUIRE(composed.size() == 3);
	}
}

TEST_CASE("filtered_string_view split function", "[split]") {
	SECTION("Basic splitting") {
		auto const& fsv = fsv::filtered_string_view{"Hello World"};
		auto const& tok = fsv::filtered_string_view{" "};
		auto const& result = fsv::split(fsv, tok);
		auto const& expected = std::vector<fsv::filtered_string_view>{"Hello", "World"};
		REQUIRE(result.size() == 2);
		REQUIRE(result[0] == "Hello");
		REQUIRE(result[1] == "World");
		REQUIRE_THAT(result, Catch::Matchers::Equals(expected));
	}

	SECTION("Empty delimiter") {
		auto const& fsv = fsv::filtered_string_view{"Hello World"};
		auto const& tok = fsv::filtered_string_view{""};
		auto const& result = fsv::split(fsv, tok);
		REQUIRE(result.size() == 1);
		REQUIRE(result[0] == fsv);
	}

	SECTION("Empty fsv") {
		auto const& fsv = fsv::filtered_string_view{""};
		auto const& tok = fsv::filtered_string_view{" "};
		auto const& result = fsv::split(fsv, tok);
		REQUIRE(result.size() == 1);
		REQUIRE(result[0] == fsv);
	}

	SECTION("Delimiter not found") {
		auto const& fsv = fsv::filtered_string_view{"Hello World"};
		auto const& tok = fsv::filtered_string_view{","};
		auto const& result = fsv::split(fsv, tok);
		REQUIRE(result.size() == 1);
		REQUIRE(result[0] == "Hello World");
		REQUIRE(result[0] == fsv);
	}

	SECTION("Consecutive delimiters") {
		auto const& fsv = fsv::filtered_string_view{"Hello,,World"};
		auto const& tok = fsv::filtered_string_view{","};
		auto const& result = fsv::split(fsv, tok);
		auto const& expected = std::vector<fsv::filtered_string_view>{"Hello", "", "World"};
		REQUIRE(result.size() == 3);
		REQUIRE(result[1].empty());
		REQUIRE_THAT(result, Catch::Matchers::Equals(expected));
	}

	SECTION("Delimiter at the start or end") {
		auto const& fsv = fsv::filtered_string_view{",Hello World,"};
		auto const& tok = fsv::filtered_string_view{","};
		auto const& result = fsv::split(fsv, tok);
		auto const& expected = std::vector<fsv::filtered_string_view>{"", "Hello World", ""};
		REQUIRE(result.size() == 3);
		REQUIRE(result[0].empty());
		REQUIRE(result[2].empty());
		REQUIRE_THAT(result, Catch::Matchers::Equals(expected));
	}

	SECTION("fsv and tok are the same") {
		auto const& fsv = fsv::filtered_string_view{"Delimiter"};
		auto const& tok = fsv::filtered_string_view{"Delimiter"};
		auto const& result = fsv::split(fsv, tok);
		// Expecting two empty strings as split results
		auto const& expected = std::vector<fsv::filtered_string_view>{"", ""};
		REQUIRE(result.size() == 2);
		REQUIRE(result[0].empty());
		REQUIRE(result[1].empty());
		REQUIRE_THAT(result, Catch::Matchers::Equals(expected));
	}

	SECTION("Underlying string are euqal") {
		auto const& fsv = fsv::filtered_string_view{"Delimiter"};
		auto const& tok = fsv::filtered_string_view{"Delimiter"};
		auto const& result = fsv::split(fsv, tok);
		REQUIRE(fsv.data() == result[0].data());
		REQUIRE(fsv.data() == result[1].data());
	}

	SECTION("Different filters for fsv and tok") {
		auto const& fsv_pred = [](const char& c) -> bool { return c != 'c'; };
		auto const& tok_pred = [](const char& c) -> bool { return c != 'g'; };

		auto const& fsv = fsv::filtered_string_view{"abcdefg", fsv_pred};
		// effectively becomes "bde"
		auto const& tok = fsv::filtered_string_view{"bgde", tok_pred};
		auto const& result = fsv::split(fsv, tok);

		REQUIRE(result.size() == 2);
		REQUIRE(result[0] == "a");
		REQUIRE(result[1] == "fg");
		REQUIRE(fsv.data() == result[0].data());
		REQUIRE(fsv.data() == result[1].data());
	}

	SECTION("Filters removing multiple characters from fsv and tok") {
		auto const& fsv_pred = [](const char& c) -> bool { return c != 'a' && c != 'e'; };
		auto const& tok_pred = [](const char& c) -> bool { return c != 'x' && c != 'z'; };

		auto const& fsv = fsv::filtered_string_view{"alexander", fsv_pred};
		// effectively becomes ""
		auto const& tok = fsv::filtered_string_view{"xz", tok_pred};
		auto const& result = fsv::split(fsv, tok);

		REQUIRE(result.size() == 1);
		REQUIRE(result[0] == "lxndr");
	}

	SECTION("Complex filters changing perception of string and delimiter") {
		auto const& fsv_pred = [](const char& c) -> bool { return c != 's' && c != 'v'; };
		auto const& tok_pred = [](const char& c) -> bool { return c != 'e' && c != 'i'; };

		auto const& fsv = fsv::filtered_string_view{"universe", fsv_pred};
		// becomes "v"
		auto const& tok = fsv::filtered_string_view{"sevi", tok_pred};
		auto const& result = fsv::split(fsv, tok);

		REQUIRE(result.size() == 1);
		REQUIRE(result[0] == "uniere");
	}
}

TEST_CASE("filtered_string_view substr function", "[substr]") {
	SECTION("Regular substring extraction") {
		auto const& sv = fsv::filtered_string_view{"Siberian Husky"};
		auto const& result = fsv::substr(sv, 9);
		REQUIRE(result == "Husky");
		REQUIRE(result.data() == sv.data());
	}

	SECTION("Substring with count") {
		auto const& is_upper = [](const char& c) -> bool { return std::isupper(static_cast<unsigned char>(c)); };
		auto const& sv = fsv::filtered_string_view{"Sled Dog", is_upper};
		auto const& result = fsv::substr(sv, 0, 2);
		REQUIRE(result == "SD");
		REQUIRE(result.data() == sv.data());
	}

	SECTION("Substring with count") {
		auto const& is_upper = [](const char& c) -> bool { return std::isupper(static_cast<unsigned char>(c)); };
		auto const& sv = fsv::filtered_string_view{"A Sled Dog", is_upper};
		auto const& result = fsv::substr(sv, 0, 2);
		REQUIRE(result == "AS");
		REQUIRE(result.data() == sv.data());
	}

	SECTION("Excessive count") {
		auto const& sv = fsv::filtered_string_view{"Excessive"};
		// count beyond the string length
		auto const& result = fsv::substr(sv, 3, 100);
		REQUIRE(result == "essive");
	}

	SECTION("Count is zero") {
		auto const& sv = fsv::filtered_string_view{"Count Zero"};
		auto const& result = fsv::substr(sv, 5, 0);
		REQUIRE(result == " Zero");
	}

	SECTION("Position at the end of the string") {
		auto const& sv = fsv::filtered_string_view{"Position End"};
		auto const& size = static_cast<int>(sv.size());
		auto const& result = fsv::substr(sv, size);
		// should return an empty string
		REQUIRE(result == "");
		// with same underlying string
		REQUIRE(result.data() == sv.data());
	}

	SECTION("Zero length string") {
		auto const& sv = fsv::filtered_string_view{""};
		// Attempt to get substr from an empty string
		auto const& result = fsv::substr(sv, 0, 1);
		// should still return an empty string
		REQUIRE(result == "");
	}

	SECTION("Underlying string are equal") {
		auto const& sv = fsv::filtered_string_view{"Hello World!"};
		auto const& result = fsv::substr(sv, 5, 1);
		REQUIRE(sv.data() == result.data());
	}
}

TEST_CASE("filtered_string_view as a bidirectional range", "[filtered_string_view]") {
	SECTION("Iterate over filtered string") {
		auto const& fsv = fsv::filtered_string_view{"puppy", [](char const& c) -> bool { return c != 'u' and c != 'y'; }};
		auto const& v = std::vector<char>{fsv.begin(), fsv.end()};
		REQUIRE(v == std::vector<char>{'p', 'p', 'p'});
	}

	SECTION("Reverse iterate over filtered string") {
		auto const& fsv = fsv::filtered_string_view{"milo", [](char const& c) -> bool { return c != 'i' and c != 'o'; }};
		auto const& v = std::vector<char>{fsv.rbegin(), fsv.rend()};
		REQUIRE(v == std::vector<char>{'l', 'm'});
	}

	SECTION("Edge cases with empty string") {
		auto const& fsv = fsv::filtered_string_view{"", [](char const&) -> bool { return true; }};
		REQUIRE(fsv.begin() == fsv.end());
		REQUIRE(fsv.cbegin() == fsv.cend());
		REQUIRE(fsv.rbegin() == fsv.rend());
		REQUIRE(fsv.crbegin() == fsv.crend());
	}

	SECTION("Check const correctness") {
		auto const& cfsv = fsv::filtered_string_view{"data", [](char const&) -> bool { return true; }};
		auto const& cv = std::vector<char>{cfsv.cbegin(), cfsv.cend()};
		REQUIRE(cv == std::vector<char>{'d', 'a', 't', 'a'});
	}

	SECTION("Check const reverse correctness") {
		auto const& cfsv = fsv::filtered_string_view{"data", [](char const&) -> bool { return true; }};
		auto const& cv = std::vector<char>{cfsv.crbegin(), cfsv.crend()};
		REQUIRE(cv == std::vector<char>{'a', 't', 'a', 'd'});
	}
}
