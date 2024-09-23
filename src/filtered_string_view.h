#ifndef COMP6771_ASS2_FSV_H
#define COMP6771_ASS2_FSV_H

#include <compare>
#include <functional>
#include <iterator>
#include <ostream>
#include <string>
#include <vector>

namespace fsv {
	/**
	 * @brief Defines a type for character filters, which determines if a character should be included.
	 *        A filter is a function that takes a single character and returns a boolean indicating
	 *        whether the character meets certain criteria. This type is typically used to create views
	 *        or perform operations on strings where only certain characters are considered.
	 *
	 * @note Filters are expected to be exception-safe. However, the behavior of the filter is defined by
	 *       the user, which means exceptions could be thrown if not properly handled within the filter
	 *       function. Users should ensure their filter functions do not throw exceptions to maintain
	 *       stability and performance of the operations using filters.
	 *       Furthermore, any function that utilizes these filter types can potentially throw exceptions
	 *       if the filter itself is not exception-safe. Users should be cautious and handle exceptions
	 *       where necessary when implementing or invoking such functions.
	 */
	using filter = std::function<bool(char const&)>;

	/**
	 * @brief Provides a read-only view on a filtered sequence of characters.
	 */
	class filtered_string_view {
		/**
		 * @brief Inner class to handle bidirectional iteration over filtered characters.
		 */
		class iter {
		 public:
			using iterator_category = std::bidirectional_iterator_tag;
			using difference_type = std::ptrdiff_t;
			using value_type = char;
			using reference = char const&;
			using pointer = void;

			/**
			 * @brief Default constructor that initializes an iterator in an undefined state without any operations that
			 * might throw.
			 */
			iter() noexcept = default;

			/**
			 * @brief Dereferences the iterator to access the character at the current position.
			 *
			 * @return A reference to the character at the current iterator position.
			 */
			auto operator*() const -> reference;

			/**
			 * @brief Provides pointer-like access to the character. Note: returns void as placeholder, no exception
			 * involved.
			 *
			 * @return A pointer to the character at the current iterator position, or nullptr if out of bounds.
			 */
			auto operator->() const noexcept -> pointer;

			/**
			 * @brief Increments the iterator to the next valid character without any complex logic that might throw..
			 *
			 * @return A reference to this iterator, updated to the next position.
			 */
			auto operator++() noexcept -> iter&;

			/**
			 * @brief Post-increment safely increments the index, the copy of current state involves primitive types
			 * only.
			 *
			 * @return A copy of the iterator before it was incremented.
			 */
			auto operator++(int) noexcept -> iter;

			/**
			 * @brief Decrements the iterator to the previous valid character without any complex logic that might
			 * throw.
			 *
			 * @return A reference to this iterator, updated to the previous position.
			 */
			auto operator--() noexcept -> iter&;

			/**
			 * @brief Post-decrement safely decrements the index, the copy of current state involves primitive types
			 * only.
			 *
			 * @return A copy of the iterator before it was decremented.
			 */
			auto operator--(int) noexcept -> iter;

			/**
			 * @brief Returns a pointer to the current character, or nullptr if the iterator is out of bounds.
			 * @note Marked as [[nodiscard]] to ensure that the result is checked for validity,
			 * preventing dereferencing of nullptr which could lead to crashes or undefined behavior.
			 *
			 * @return A pointer to the character at the current iterator position, or nullptr if the iterator is beyond
			 * the valid range.
			 */
			[[nodiscard]] auto get_pointer() const -> char const*;

			// Compares two iterators to determine if they point to the same position.
			friend auto operator==(iter const& lhs, iter const& rhs) noexcept -> bool;

		 private:
			// Index of the current character within the filtered view.
			int index_;
			// Pointer to the associated filtered_string_view.
			fsv::filtered_string_view const* fsv_;

			/**
			 * @brief Constructs an iterator pointing to a specific index within a filtered_string_view, simple
			 * assignment, no exception risk.
			 *
			 * @param index The position in the filtered string to point to.
			 * @param fsv Pointer to the filtered_string_view the iterator will belong to.
			 */
			iter(int const& index, filtered_string_view const* fsv) noexcept;

			/**
			 * @brief Grants filtered_string_view full access to private members of iter.
			 *
			 * This allows filtered_string_view to directly manage iterators, optimizing operations
			 * such as initialization, copying, and destruction by accessing internal details of the iter class.
			 */
			friend class filtered_string_view;
		};

	 public:
		using const_iterator = iter;
		using iterator = const_iterator;
		using const_reverse_iterator = std::reverse_iterator<const_iterator>;
		using reverse_iterator = std::reverse_iterator<iterator>;

		/**
		 * @brief Default predicate that accepts all characters.
		 */
		static filter default_predicate;

		// =============================================================================================================
		//                              CLASS CONSTRUCTOR FUNCTION
		// =============================================================================================================
		/**
		 * @brief Constructs an empty filtered_string_view.
		 * This constructor is noexcept because it only initializes pointers and size to zero, which does not involve
		 * any operations that could throw exceptions.
		 */
		filtered_string_view() noexcept;

		/**
		 * @brief Constructs a filtered_string_view from a std::string with an optional character filter.
		 * This constructor is noexcept assuming that the predicate itself is noexcept. It initializes the view based on
		 * the provided std::string, which does not throw exceptions unless a custom predicate is provided that throws.
		 *
		 * @param str The string to create a view of.
		 * @param predicate A function to determine if a character should be included in the view. Default is the
		 * default_predicate.
		 */
		filtered_string_view(std::string const& str, filter predicate = default_predicate) noexcept;

		/**
		 * @brief Constructs a filtered_string_view from a C-style string with an optional character filter.
		 * Similar to the std::string constructor, this is noexcept under the assumption that the predicate does not
		 * throw. It simply assigns the C-style string pointer and length to the internal members.
		 *
		 * @param str The C-style string to create a view of.
		 * @param predicate A function to determine if a character should be included in the view. Default is the
		 * default_predicate.
		 */
		filtered_string_view(char const* str, filter predicate = default_predicate) noexcept;

		/**
		 * @brief Default copy constructor.
		 * The copy constructor is noexcept as it only involves copying pointers and size, which are basic operations
		 * that do not throw exceptions.
		 */
		filtered_string_view(filtered_string_view const& other) = default;

		/**
		 * @brief Move constructor.
		 * The move constructor is noexcept because it transfers ownership of resources (like pointers and size) from
		 * the source object without any operations that might throw exceptions.
		 */
		filtered_string_view(filtered_string_view&& other) noexcept;

		/**
		 * @brief Default destructor.
		 * The destructor is marked noexcept as it does not perform any operations that could throw exceptions; it
		 * simply releases resources without additional processing.
		 */
		~filtered_string_view() noexcept = default;

		// =============================================================================================================
		//                               CLASS MEMBER OPERATOR FUNCTION
		// =============================================================================================================
		/**
		 * @brief Copy assignment operator.
		 */
		auto operator=(filtered_string_view const& other) -> filtered_string_view& = default;

		/**
		 * @brief Move assignment operator.
		 * Marked as noexcept because it involves transferring ownership of resources without performing any operation
		 * that might throw exceptions.
		 */
		auto operator=(filtered_string_view&& other) noexcept -> filtered_string_view&;

		/**
		 * @brief Accesses the character at a given index with bounds checking.
		 * @note Marked as [[nodiscard]] because the returned character value is critical for correctness of data
		 * handling and ignoring it could mean missing a vital check or operation on the data.
		 *
		 * @param n Index of the character to access.
		 * @return The character at the specified index.
		 */
		[[nodiscard]] auto operator[](int const& n) const -> char const&;

		/**
		 * @brief Converts the filtered view to a std::string.
		 * @note Marked as [[nodiscard]] to ensure that the resultant std::string is used, as ignoring this conversion
		 * would typically waste computational resources and potentially miss important transformations or data
		 * retrieval.
		 */
		[[nodiscard]] explicit operator std::string() const;

		// =============================================================================================================
		//                                    CLASS MEMBER FUNCTION
		// =============================================================================================================
		/**
		 * @brief Provides bounds-checked access to the character at a given index.
		 * @note Marked as [[nodiscard]] because ignoring the returned character could bypass important data validation
		 * or transformation steps.
		 *
		 * @param index Index of the character to access.
		 * @return The character at the specified index.
		 * @throw std::out_of_range if the index is out of bounds.
		 */
		[[nodiscard]] auto at(int const& index) const -> char const&;

		/**
		 * @brief Returns the number of characters in the filtered view.
		 * @note Marked as [[nodiscard]] to ensure that conditions based on the size are always checked, preventing
		 * logical errors such as unnecessary processing when the view is actually empty.
		 *
		 * @return Number of characters.
		 */
		[[nodiscard]] auto size() const -> std::size_t;

		/**
		 * @brief Checks if the filtered view is empty.
		 *
		 * @return True if the view is empty, false otherwise.
		 */
		[[nodiscard]] auto empty() const -> bool;

		/**
		 * @brief Returns a pointer to the underlying character data.
		 * Returning a pointer is a straightforward operation and does not involve any operations that might throw
		 * exceptions.
		 * @note Marked as [[nodiscard]] because the returned data pointer is essential for direct data access,
		 * and ignoring it could lead to missed opportunities to interact with the underlying data buffer directly.
		 *
		 * @return Pointer to the data.
		 */
		[[nodiscard]] auto data() const noexcept -> const char*;

		/**
		 * @brief Returns the filter used for character inclusion.
		 * Returning a copy of the filter function object is straightforward and should not throw, assuming the copy
		 * operation for std::function is exception-safe.
		 * @note Marked as [[nodiscard]] because the predicate defines how the view filters characters,
		 * and ignoring it can lead to a misunderstanding of how data is being processed within the view.
		 *
		 * @return The filter function.
		 */
		[[nodiscard]] auto predicate() const noexcept -> const filter&;

		// =============================================================================================================
		//                                    CLASS OPERATOR FUNCTION
		// =============================================================================================================
		// Compares two filtered_string_views for equality.
		friend auto operator==(filtered_string_view const& lhs, filtered_string_view const& rhs) -> bool;
		// Compares two filtered_string_views lexicographically.
		friend auto operator<=>(filtered_string_view const& lhs, filtered_string_view const& rhs) -> std::strong_ordering;
		// Enables insertion of the filtered_string_view into an output stream.
		friend auto operator<<(std::ostream& os, filtered_string_view const& obj) -> std::ostream&;

		// =============================================================================================================
		//                                    RANGE ACCESS FUNCTIONS
		// =============================================================================================================
		/**
		 * @note All range functions marked as [[nodiscard]] because ignoring the returned iterator can lead to errors
		 * in iterating over the view, potentially skipping necessary processing of the view's elements.
		 */
		/**
		 * @brief Returns an iterator to the beginning of the filtered view.
		 * Marked as noexcept because it constructs an iterator using an index which is a trivial operation that does
		 * not throw exceptions.
		 *
		 * @return An iterator pointing to the first character of the filtered view.
		 */
		[[nodiscard]] auto begin() const noexcept -> iterator;

		/**
		 * @brief Returns an iterator to the end of the filtered view.
		 *
		 * @return An iterator pointing to the past-the-end character of the filtered view.
		 */
		[[nodiscard]] auto end() const -> iterator;

		/**
		 * @brief Returns a const_iterator to the beginning of the filtered view. Equivalent to begin().
		 * Marked as noexcept for the same reasons as begin().
		 *
		 * @return A const_iterator pointing to the first character of the filtered view.
		 */
		[[nodiscard]] auto cbegin() const noexcept -> const_iterator;

		/**
		 * @brief Returns a const_iterator to the end of the filtered view. Equivalent to end().
		 *
		 * @return A const_iterator pointing to the past-the-end character of the filtered view.
		 */
		[[nodiscard]] auto cend() const -> const_iterator;

		/**
		 * @brief Returns a reverse_iterator to the beginning of the reversed filtered view.
		 *
		 * @return A reverse_iterator pointing to the first character of the reversed filtered view.
		 */
		[[nodiscard]] auto rbegin() const -> reverse_iterator;

		/**
		 * @brief Returns a reverse_iterator to the end of the reversed filtered view. Equivalent to rend().
		 * Marked as noexcept because it constructs a reverse_iterator based on begin(), which is a trivial operation
		 * that does not throw exceptions.
		 *
		 * @return A reverse_iterator pointing to the past-the-end character of the reversed filtered view.
		 */
		[[nodiscard]] auto rend() const noexcept -> reverse_iterator;

		/**
		 * @brief Returns a const_reverse_iterator to the beginning of the reversed filtered view. Equivalent to
		 * rbegin().
		 *
		 * @return A const_reverse_iterator pointing to the first character of the reversed filtered view.
		 */
		[[nodiscard]] auto crbegin() const -> const_reverse_iterator;

		/**
		 * @brief Returns a const_reverse_iterator to the end of the reversed filtered view. Equivalent to rend().
		 * Marked as noexcept for the same reasons as rend().
		 *
		 * @return A const_reverse_iterator pointing to the past-the-end character of the reversed filtered view.
		 */
		[[nodiscard]] auto crend() const noexcept -> const_reverse_iterator;

		// =============================================================================================================
		//                          NON-MEMBER UTILITY FUNCTIONS
		// =============================================================================================================
		// Allows splitting the filtered_string_view based on a delimiter.
		friend auto
		split(filtered_string_view const& fsv, filtered_string_view const& tok) -> std::vector<filtered_string_view>;
		// Allows extracting a substring from the filtered_string_view.
		friend auto substr(filtered_string_view const& fsv, int const& pos, int const& count) -> filtered_string_view;

	 private:
		// Holds the pointer to the start of the character data.
		char const* data_;
		// Holds the length of the character data.
		std::size_t length_;
		// Holds the predicate used to determine which characters are included in the view.
		filter predicate_;

		/**
		 * @brief Private constructor used by public constructors to initialize the filtered_string_view.
		 * This function is noexcept because it only assigns values to internal variables and does not involve any
		 * operations that could throw exceptions.
		 *
		 * @param str Pointer to the character data to be used in the view.
		 * @param length The number of characters in the data.
		 * @param predicate The filter predicate to apply to the data.
		 */
		filtered_string_view(char const* str, std::size_t const& length, filter predicate = default_predicate) noexcept;

		/**
		 * @brief Finds the pointer to the character at a specific index, considering the filter.
		 * @note Using [[nodiscard]] emphasizes the importance of the result, as missing to check the pointer could lead
		 * to erroneous operations or logic based on unfiltered or out-of-bound character data.
		 *
		 * @param index The index of the character to find.
		 * @return Pointer to the character if it passes the filter and is within the range, otherwise nullptr.
		 */
		[[nodiscard]] auto find_filtered_char(int const& index) const -> char const*;

		/**
		 * @brief Generates a filter predicate for a specific range within the filtered string.
		 * This function is not marked as noexcept because the returned predicate might invoke a user-defined
		 * function that could potentially throw exceptions.
		 * @note The use of [[nodiscard]] underlines the necessity to utilize the returned predicate for intended data
		 * filtering, ensuring that the subrange operations are correctly set up with the new filtering criteria.
		 *
		 * @param start_pos The starting position of the range within the data.
		 * @param end_pos The ending position of the range within the data.
		 * @return A filter predicate that tests if a character belongs to the specified range and passes the original
		 * filter.
		 */
		[[nodiscard]] auto make_range_predicate(long const& start_pos, long const& end_pos) const -> filter;
	};

	// =================================================================================================================
	//                                    CLASS OPERATOR FUNCTION
	// =================================================================================================================
	/**
	 * @brief Checks for equality between two filtered_string_views, comparing character by character.
	 * @note Marked as [[nodiscard]] because the result of the comparison is essential to ensure logical correctness,
	 * especially when used in conditional expressions.
	 *
	 * @param lhs The left-hand side filtered_string_view for comparison.
	 * @param rhs The right-hand side filtered_string_view for comparison.
	 * @return True if both filtered_string_views are equal, otherwise false.
	 */
	[[nodiscard]] auto operator==(filtered_string_view const& lhs, filtered_string_view const& rhs) -> bool;

	/**
	 * @brief Compares two filtered_string_views lexicographically.
	 * @note Marked as [[nodiscard]] because the comparison result is critical for sorting and ordering operations,
	 * and ignoring this could lead to incorrect data handling or order logic.
	 *
	 * @param lhs The left-hand side filtered_string_view for comparison.
	 * @param rhs The right-hand side filtered_string_view for comparison.
	 * @return A std::strong_ordering result of the comparison.
	 */
	[[nodiscard]] auto
	operator<=>(filtered_string_view const& lhs, filtered_string_view const& rhs) -> std::strong_ordering;

	/**
	 * @brief Inserts the characters of the filtered_string_view into an output stream.
	 *
	 * @param os The output stream to write to.
	 * @param obj The filtered_string_view to insert into the stream.
	 * @return Reference to the modified output stream.
	 */
	auto operator<<(std::ostream& os, filtered_string_view const& obj) -> std::ostream&;

	// =================================================================================================================
	//                                   ITERATOR OPERATOR FUNCTION
	// =================================================================================================================
	/**
	 * @brief Compares indices and view pointers, basic equality checks with no exception risk.
	 * Marked as noexcept because swapping pointers and sizes is a noexcept operation.
	 * @note Marked as [[nodiscard]] because ignoring the result of an iterator comparison could lead to incorrect
	 *
	 * @param lhs The left-hand side iterator to compare.
	 * @param rhs The right-hand side iterator to compare.
	 * @return True if both iterators point to the same position in the same filtered_string_view.
	 */
	[[nodiscard]] auto
	operator==(filtered_string_view::iterator const& lhs, filtered_string_view::iterator const& rhs) noexcept -> bool;

	// =================================================================================================================
	//                                      NON-MEMBER FUNCTION
	// =================================================================================================================
	/**
	 * @brief Combines multiple filtering conditions into a single predicate and returns a new filtered_string_view.
	 *
	 * @param fsv The original filtered_string_view.
	 * @param filts A vector of filter functions that will be applied in a logical AND sequence.
	 * @return A new filtered_string_view that uses the combined filter.
	 */
	auto compose(filtered_string_view const& fsv, std::vector<filter> const& filts) -> filtered_string_view;

	/**
	 * @brief Splits the filtered string based on a specified delimiter, returning a vector of substrings.
	 * @note Marked as [[nodiscard]] because the function's purpose is to generate a new set of substrings which are
	 * essential for further processing or analysis.
	 *
	 * @param fsv The filtered_string_view to split.
	 * @param tok The delimiter to use for splitting.
	 * @return A vector of filtered_string_view each representing a part of the original view split by the
	 * delimiter.
	 */
	[[nodiscard]] auto
	split(filtered_string_view const& fsv, filtered_string_view const& tok) -> std::vector<filtered_string_view>;

	/**
	 * @brief Extracts a substring from the filtered view based on a specified starting position and length.
	 * @note The function is marked with [[nodiscard]] because ignoring the returned substring can lead to ignoring
	 *       critical parts of data processing, especially when the intent is to process or use the extracted substring.
	 *
	 * @param fsv The filtered_string_view to extract from.
	 * @param pos The starting index of the substring within the filtered view.
	 * @param count The number of characters to include in the substring.
	 * @return A filtered_string_view representing the specified substring.
	 * @throws std::out_of_range If the starting position is outside the valid range of the filtered view.
	 */
	[[nodiscard]] auto
	substr(filtered_string_view const& fsv, int const& pos = 0, int const& count = 0) -> filtered_string_view;

} // namespace fsv

#endif // COMP6771_ASS2_FSV_H
