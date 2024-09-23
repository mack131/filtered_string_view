# Assignment 2 - `filtered_string_view`

## Changelog:
* 19/06: Updated the testing clarity criteria

[toc]

## 1. Introduction
_Note: this section is copied almost verbatim from [totw/1](https://abseil.io/tips/1)._

A `string_view` can be thought of as a “view” into an existing character buffer. Specifically, a `string_view` consists of only a pointer and a length, identifying a section of character data that is not owned by the `string_view` and cannot be modified by the view.

### 1.1. What's the point?
This section isn't relevant to the actual implementation of the assignment, but is useful if you want to understand the motivation behind why we're asking you to write this assignment.

When creating a function to take a (constant) string as an argument, programmers often opt for one of the following ways:

```c++
// C Convention
void takes_char_star(const char *s);

// Old Standard C++ convention
void takes_string(const std::string &s);
```

These work fine when a caller has the string in the format already provided, but what happens when a conversion is needed (either from `const char *` to `std::string` or `std::string` to `const char *`)?

Callers needing to convert a `std::string` to a `const char *` need to use the (efficient and inconvenient) `c_str()` function:

```c++
void already_had_string(const std::string &s) {
  takes_char_star(s.c_str()); // explicit conversion
}
```

Callers needing to convert a `const char *` to a `std::string` don’t need to do anything additional (the good news) but will invoke the creation of a (convenient but inefficient) temporary string, copying the contents of that string (the bad news):

```c++
void already_has_char_star(const char *s) {
  takes_string(s); // compiler will make a copy
}
```

The solution is a `string_view`. `string_view` has implicit conversion constructors from both `const char *` and `const std::string &`, and since `string_view` doesn’t copy, there is no `O(n)` memory penalty for making a hidden copy. In the case where a `const std::string &` is passed, the constructor runs in `O(1)` time. In the case where a `const char *` is passed, the constructor will calculate the length for you (or you can use the two-parameter `string_view` constructor which takes in the length of the string).

## 2. The Task
Write a more complex version of the `string_view` described above - the `filtered_string_view`.

A `filtered_string_view` is like a `string_view`, however it presents a filtered view of the underlying data. This means that readers of the `filtered_string_view` may only see part of the underlying data.

The filter is optionally provided by the caller as a predicate (a function which returns a boolean) which returns `true` if the data is to be kept. If not provided, the filter will default to the "true" predicate, i.e., a function which always returns true. In this case, no data would be filtered.

You can see examples of this behaviour throughout the spec.

You'll also be writing an iterator for this class. The iterator will be a bidirectional `const_iterator` (as a `string_view` provides a read only view into its underlying data).

You will implement this specification in `src/filtered_string_view.h` and/or `src/filtered_string_view.cpp`, and write tests in `src/filtered_string_view.test.cpp`.

### 2.1. Required Internal Representation and Terminology
As the `filtered_string_view` is not intended to own the underlying string data it's pointing to, it's sufficient to represent the data required by using a single raw `const char *` and a `std::size_t` length (which is the length of the underlying string).

In the spec we will refer to the raw pointer as simply the **pointer**.

You will also need to store the predicate function. You should store it as a [`std::function<bool(const char &)>`](https://en.cppreference.com/w/cpp/utility/functional/function). We will refer to this member as the **predicate**. For convenience, we have added a helper type alias `filter` which is equivalent to the aforementioned predicate type.

`filtered_string_view` is intended to be lightweight and cheap to copy. Therefore, you cannot store any other non-static data members. We are opting for space savings instead of time savings in the complexity trade-off.

In the constructors, operators, methods and functions described below, references to the lengths and indexes of the `filtered_string_view` always refer to the **filtered** data. Any characters which are filtered out are essentially invisible - except through `.data()`, callers should never be able to see any indication that these characters exist.

We will extensively use the term **filtered string**. This is the "virtual" string that is conceptually the result of the taking the underlying string data and applying the filtering predicate to it to filter out characters. Note that we never actually modify and store this filtered string anywhere.

----

### 2.2. Namespace `fsv` Provided Members

```cpp
namespace fsv {
  using filter = std::function<bool(const char &)>
}
```
A convenience alias for the predicate type. You must **NOT** modify this alias.

### 2.3. Static Data Members
```cpp
filter filtered_string_view::default_predicate;
```

A static data member used as the default predicate when none is provided.

It is the true function, a function that returns `true` for any character passed in.
If correctly implemented, the following code snippet should run without error:
```cpp
for (char c = std::numeric_limits<char>::min(); c != std::numeric_limits<char>::max(); c++) {
  assert(fsv::filtered_string_view::default_predicate(c));
}
```

----

### 2.4. Constructors

Note: It's very important your constructors work. If we can't validly construct your object, we can't validly test anything either.

Hint: the minimum number of constructor prototypes (not including the copy & move constructors) you must write is three, not five (why this might be the case is left as an exercise to the reader).

#### 2.4.1. Default Constructor
```cpp
filtered_string_view();
```

Constructs a `filtered_string_view` with the pointer set to `nullptr`, the length set to `0`, and the predicate set to the `true` predicate.

##### Examples
```cpp
auto sv = fsv::filtered_string_view{};
std::cout << sv.size() << std::endl;
```

Output: `0`


#### 2.4.2. Implicit String Constructor

```cpp
filtered_string_view(const std::string &str);
```

Constructs a `filtered_string_view` with the pointer set to the string's underlying data, and the predicate set to the `true` predicate.

Hint: you can obtain the underlying data of a `std::string` by calling `.data()` or `.c_str()`.

Hint: This constructor should **not** be `explicit` because it's actually desirable to enable implicit conversions from `std::string` to `filtered_string_view`.


##### Examples

```cpp
auto s = std::string{"cat"};
auto sv = fsv::filtered_string_view{s};
std::cout << sv.size() << std::endl;
```

Output: `3`

#### 2.4.3. String Constructor with Predicate

```cpp
filtered_string_view(const std::string &str, filter predicate);
```

Same as the Implicit String Constructor, but with the predicate set to the given one.

##### Examples

```cpp
auto s = std::string{"cat"};
auto pred = [](const char& c) { return c == 'a'; };
auto sv = fsv::filtered_string_view{s, pred};
std::cout << sv.size() << std::endl;
```

Output: `1`

#### 2.4.4. Implicit Null-Terminated String Constructor

```cpp
filtered_string_view(const char *str);
```

Constructs a `filtered_string_view` with the pointer set to the given null-terminated string, and the predicate set to the `true` predicate.

Hint: This constructor should be implicit because it's actually desirable to enable implicit conversions from `const char *` to `filtered_string_view`.

Note: You can assume that the passed in string pointer is validly null-terminated (the final character in the string is `'\0'`).

##### Examples

```cpp
auto sv = fsv::filtered_string_view{"cat"};
std::cout << sv.size() << std::endl;
```

Output: `3`

#### 2.4.5. Null-Terminated String with Predicate Constructor

```cpp
filtered_string_view(const char *str, filter predicate);
```

Same as the implicit null-terminated string pointer constructor, but with the predicate set to the given one.

##### Examples

```cpp
auto pred = [](const char &c) { return c == 'a'; };
auto sv = fsv::filtered_string_view{"cat", pred};
std::cout << sv.size();
```

Output: `1`

#### 2.4.6. Copy and Move Constructors

```cpp
/* 1 */ filtered_string_view(const filtered_string_view &other);
/* 2 */ filtered_string_view(filtered_string_view &&other);
```

For (1) above (the copy constructor):
- The newly constructed object must compare equal to the copied object.
- A member-wise copy is sufficient.
- The underlying character buffer should not be deep-copied.

For (2) above (the move constructor):
- The newly constructed object must be identical to the moved-from object before it was moved.
- The moved from object should be in the same state as a default constructed `fsv::filtered_string_view`.

##### Examples

```cpp
auto sv1 = fsv::filtered_string_view{"bulldog"};
const auto copy = sv1;

assert(copy.data() == sv1.data()); // pointers compare equal.

const auto move = std::move(sv1);
assert(sv1.data() == nullptr); // true: sv1's guts were moved into `move`
```

----

### 2.5. Destructor
```cpp
~filtered_string_view();
```

**Important**: You must explicitly declare the destructor as default.

----

### 2.5.1. Member Operators

The table below intentionally omits the full signature of these operator overloads.
You will need to figure out and complete these yourself.

**Hint**: you may wish to review the [canonical operator overloading](https://en.cppreference.com/w/cpp/language/operators) reference and associated lecture material.

#### 2.5.2. Copy Assignment

```cpp
operator=(const filtered_string_view &other);
```

Copies at least the length, data and predicate of `other` so that after the copy-assignment this `filtered_string_view` and `other` compare equal via `operator==`.

In the case of self-copy, the object should remain unchanged.

##### Examples
```cpp
auto pred = [](const char &c) { return c == '4' || c == '2'; };
auto fsv1 = fsv::filtered_string_view{"42 bro", pred};
auto fsv2 = fsv::filtered_string_view{};
fsv2 = fsv1;
assert(fsv1 == fsv2);
```

#### 2.5.3. Move Assignment
```cpp
operator=(filtered_string_view &&other);
```

The moved from object should be left in a valid state equivalent to a default-constructed `filtered_string_view`, except in the case of self-assignment, in which the moved from object should remain unchanged.

##### Examples
```cpp
auto pred = [](const char &c) { return c == '8' || c == '9'; };
auto fsv1 = fsv::filtered_string_view{"'89 baby", pred};
auto fsv2 = fsv::filtered_string_view{};

fsv2 = std::move(fsv1);

assert(fsv1.size() == 0 && fsv1.data() == nullptr);
```

#### 2.5.4. Subscript
```cpp
auto operator[](int n) -> const char &;
```
Allows reading a character from the **filtered string** given its index.

Hint: Remember that the underlying string data should be read-only, and it should not be possible for clients of `filtered_string_view` to mutate it through the class.
 
##### Examples

```cpp
auto pred = [](const char &c) { return c == '9' || c == '0' || c == ' '; };
auto fsv1 = fsv::filtered_string_view{"only 90s kids understand", pred};
std::cout << fsv1[2] << std::endl;
```

Output: `0`

#### 2.5.5. String Type Conversion
```cpp
explicit operator std::string();
```

Enables type casting a `filtered_string_view` to a `std::string`. The returned `std::string` should be a copy of the filtered string, so any characters which are filtered out should not be present. The returned `std::string` must be a copy so that modifications to it do not mutate the underyling data backing the `filtered_string_view`.

##### Examples

```cpp
auto sv = fsv::filtered_string_view("vizsla");
auto s = static_cast<std::string>(sv);
std::cout << std::boolalpha << (sv.data() == s.data()) << std::endl;
```

Output: `false`

----

### 2.6. Member Functions

#### 2.6.1. at
```cpp
auto at(int index) -> const char &;
```

Allows reading a character from the **filtered string** given its index.

An index is valid if and only if
```cpp
assert(0 <= index < size());
```
holds. This means no index is valid when `size() == 0`.

Returns:
- the character at `index` in the **filtered string** if the index is valid.

Throws:
- a `std::domain_error{"filtered_string_view::at(<index>): invalid index"}`, where `<index>` should be replaced with the actual index passed in if the index is invalid.

Hint: Remember that the underlying string data should be read-only, and it should not be possible for clients of `filtered_string_view` to mutate it through the class.

##### Examples

```cpp
auto vowels = std::set<char>{'a', 'A', 'e', 'E', 'i', 'I', 'o', 'O', 'u', 'U'};
auto is_vowel = [&vowels](const char &c){ return vowels.contains(c); };
auto sv = fsv::filtered_string_view{"Malamute", is_vowel};
```

Output: `a`

```cpp
auto sv = fsv::filtered_string_view{""};
try {
  sv.at(0);
} catch (const std::domain_error &e) {
  std::cout << e.what();
}
```

Output: `filtered_string_view::at(0): invalid index`

#### 2.6.2. size
```cpp
auto size() -> std::size_t;
```

Returns the size of the filtered string.

##### Examples

```cpp
auto sv = fsv::filtered_string_view{"Maltese"};
std::cout << sv.size();
```

Output: `7`

```cpp
auto sv = fsv::filtered_string_view{"Toy Poodle", [](const char &c) {
    return c == 'o';
}};
std::cout << sv.size();
```

Output: `3`

#### 2.6.3. empty
```cpp
auto empty() -> bool;
```

Returns whether the filtered string is empty.
The filtered string is "empty" when `size() == 0`.

##### Examples

```cpp
auto sv = fsv::filtered_string_view{"Australian Shephard"};
auto empty_sv = fsv::filtered_string_view{};
std::cout << std::boolalpha << sv.empty() << ' ' << empty_sv.empty();
```

Output: `false true`

```cpp
auto sv = fsv::filtered_string_view{"Border Collie", [](const char &c) {
    return c == 'z';
}};
std::cout << std::boolalpha << sv.empty();
```

Output: `true`

#### 2.6.4. data
```cpp
auto data() -> const char *;
```

Returns a pointer to the underlying data backing the `filtered_string_view`. Filtering behaviour is ignored in this function.

##### Examples

```cpp
auto s = "Sum 42";
auto sv = fsv::filtered_string_view{s, [](const char &c){ return false; }};
for (auto ptr = sv.data(); *ptr; ++ptr) {
  std::cout << *ptr;
}
```

Output: `Sum 42`

#### 2.6.5. predicate
```cpp
auto predicate() -> const filter&;
```
Allows access to the predicate used to do filtering.

##### Examples 

```cpp
const auto print_and_return_true = [](const char &) {
    std::cout << "hi!";
    return true;
};
const auto s = fsv::filtered_string_view{"doggo", print_and_return_true};

const auto& predicate = s.predicate();
predicate(char{});
```
Output: `hi!`
    
----

### 2.7. Non-Member Operators

#### 2.7.1. Equality Comparison
```cpp
auto operator==(const filtered_string_view &lhs, const filtered_string_view &rhs) -> bool;
```
Lexicographically compares two `filtered_string_view`s for equality.
The predicate function *does not* participate in equality directly: only the two filtered strings should be lexicographically compared.

When this operator is correctly implemented, it should be possible to use
- `operator!=`

without writing any extra code.

##### Examples

```cpp
auto const lo = fsv::filtered_string_view{"aaa"};
auto const hi = fsv::filtered_string_view{"zzz"};

std::cout << std::boolalpha 
          << (lo == hi) << ' '
          << (lo != hi);
```
Output: `false true`

#### 2.7.2. Relational Comparison

```cpp
auto operator<=>(const filtered_string_view &lhs, const filtered_string_view &rhs) -> std::strong_ordering;
```

Uses the C++20 spaceship operator to lexicographically compare two `filtered_string_view`s.
The predicate function *does not* participate in comparison directly: only the two filtered strings should be lexicographically compared.

When this operator is correctly implemented, it should be possible to use the other relational operators
- `operator<`
- `operator>`
- `operator<=`
- `operator>=`

without writing any extra code.

##### Examples

```cpp
auto const lo = fsv::filtered_string_view{"aaa"};
auto const hi = fsv::filtered_string_view{"zzz"};

std::cout << std::boolalpha 
          << (lo < hi) << ' '
          << (lo <= hi) << ' '
          << (lo > hi) << ' '
          << (lo >= hi) << ' '
          << (lo <=> hi == std::strong_ordering::less);
```
Output: `true true false false true`

#### 2.7.3. Output Stream

```cpp
auto operator<<(std::ostream &os, const filtered_string_view &fsv) -> std::ostream&;
```

Prints to `os` the characters of the filtered string in `fsv`.

There is no newline at the end.

##### Examples

```cpp
auto fsv = fsv::filtered_string_view{"c++ > rust > java", [](const char &c){ return c == 'c' || c == '+'; }};
std::cout << fsv;
```

Output: `c++`

### 2.8. Non-Member Utility Functions

#### 2.8.1. compose
```cpp
auto compose(const filtered_string_view &fsv, const std::vector<filter> &filts) -> filtered_string_view;
```
Accepts a `filtered_string_view` and a vector of filtering predicates and returns a new `filtered_string_view` which filters the same underlying string as `fsv` but with a predicate which only returns `true` if all of the filters in `filts` (called in order of the vector from left to right) also return `true`, that is, all the filters would have filtered the same letter. If any filter in the chain of function calls returns `false`, the new predicate should short-circuit and **not** call the subsequent functions and return `false`.

This is akin to a logical AND. In the expression
```cpp
bool b0 = true, b1 = false, b2 = true;
b0 && b1 && b2;
```
`b0` and `b1` will be evaluated, but since `b1` is `false`, `b2` won't be evaluated. This expression has "short-circuited".

**Note**: `fsv`'s underlying string will always be null-terminated when calling this function, so it is possible to determine its length with `std::strlen()`.

##### Examples
```cpp
auto best_languages = fsv::filtered_string_view{"c / c++"};
auto vf = std::vector<filter>{
  [](const char &c){ return c == 'c' || c == '+' || c == '/'; },
  [](const char &c){ return c > ' '; },
  [](const char &c){ return true; }
};

auto sv = fsv::compose(best_languages, vf);
std::cout << sv;
```

Output: `c/c++`

#### 2.8.2. split

```cpp
auto split(const filtered_string_view &fsv, const filtered_string_view &tok) -> std::vector<filtered_string_view>;
```
Splits `fsv` on the delimiter in `tok` into a vector of substrings. This can be done by initialising each slice of the split with the original underlying data of `fsv` and a new predicate which calculates the extent of the substring within the original string.

Usually, the delimiter appears in the middle of two strings. It is, however, possible that the delimiter appears only on the left or the right of a split, and not the middle. In these cases, the parts before/after the delimiter is equivalent to the `filtered_string_view("")`. Furthermore, the delimiter should not be a part of any of the split slices.

If `tok` does not appear in `fsv`, returns a vector of a single element which is a copy of the original `fsv`. For our purposes, the empty string `""` *never* appears inside of `fsv`.

Similarly, if `fsv` is empty, returns a vector of a single element which is a copy of the original `fsv`.

**Hint**: `split()` is intended to mirror the same semantics as Python3's `split()`. If you are unsure if you have implemented `split()` correctly, you can use `python3` to check your answer.
- Note that Python's version of `split()` does not accept an empty delimiter, whereas `fsv::split()` does. Be careful with this.

##### Examples
```cpp
auto interest = std::set<char>{'a', 'A', 'b', 'B', 'c', 'C', 'd', 'D', 'e', 'E', 'f', 'F', ' ', '/'};
auto sv = fsv::filtered_string_view{"0xDEADBEEF / 0xdeadbeef", [&interest](const char &c){ return interest.contains(c); }};
auto tok = fsv::filtered_string_view{" / "};
auto v = fsv::split(sv, tok);

std::cout << v[0] << " " << v[1];
```

Output: `DEADBEEF deadbeef`

```cpp
auto sv = fsv::filtered_string_view{"xax"};
auto tok  = fsv::filtered_string_view{"x"};
auto v = fsv::split(fsv, tok);
auto expected = std::vector<fsv::filtered_string_view>{"", "a", ""};

CHECK(v == expected);
```

```cpp
auto sv = fsv::filtered_string_view{"xx"};
auto tok  = fsv::filtered_string_view{"x"};
auto v = fsv::split(sv, tok);
auto expected = std::vector<fsv::filtered_string_view>{"", "", ""};

CHECK(v == expected);
```

#### 2.8.3. substr
```cpp
auto substr(const filtered_string_view &fsv, int pos = 0, int count = 0) -> filtered_string_view;
```

Returns a new `filtered_string_view` with the same underlying string as `fsv` which presents a "substring" view. The substring begins at `pos` and has length `rcount`, where `rcount = count <= 0 ? size() - pos() : count`. That is, it provides a view into the substring `[pos, pos + rcount)` of `fsv`.

**Note**: it is possible to have a substring of length 0. In that case, the returned `filtered_string_view` should be equivalent to `""`.

##### Examples
```cpp
auto sv = fsv::filtered_string_view{"Siberian Husky"};
std::cout << fsv::substr(sv, 9);
```
Output: `Husky`

```cpp
auto is_upper = [](const char &c) { return std::isupper(static_cast<unsigned char>(c)};
auto sv = fsv::filtered_string_view{"Sled Dog", is_upper};
std::cout << fsv::substr(sv, 0, 2);
```
Output: `SD`

----

### 2.9. Iterator

You must define an iterator for this class. Conceptually, the iterator allows callers to iterate through the filtered string character by character.

You need to implement only a bidirectional `const_iterator` -- this means that even with a non-constant `filtered_string_view` it is impossible to mutate the underlying filtered_string. There are other examples of this kind of single-iterator class in the Standard Library, for example, [`std::set`](https://en.cppreference.com/w/cpp/container/set).

Here are a few hints to defining your iterator correctly:
- It should be tagged as a [bidirectional iterator](https://en.cppreference.com/w/cpp/named_req/BidirectionalIterator), and it should support all the operators that a bidirectional iterator would support.
- The `value_type` should be `char`.
- The `reference` type alias should be `const char &`.
- The `pointer` type alias should be `void`.
- It should have a public default constructor.
- The iterator should not allow mutation into the underlying data.
- We have provided part of the interface in the header file for you to start working from.

You may also define private, implementation specific constructors.

You may assume that any methods that mutate the original `filtered_string_view` invalidates any iterators. 

Your main `filtered_string_view` class should have a member type alias `iterator = const_iterator`.

#### Examples
```cpp
auto print_via_iterator = [](fsv::filtered_string_view const& sv) {
  std::copy(sv.begin(), sv.end(), std::ostream_iterator<char>(std::cout, " "));
}

// With default predicate:
auto fsv1 = fsv::filtered_string_view{"corgi"};
print_via_iterator(fsv1);
```
Output: `c o r g i`

```cpp
// With predicate which removes lowercase vowels:
auto fsv = fsv::filtered_string_view{"samoyed", [](const char &c) {
  return !(c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u');
}};
auto it = fsv.begin();
std::cout << *it << *std::next(it)  << *std::next(it, 2) << *std::next(it, 3);
```
Output: `smyd`

```cpp
const auto str = std::string("tosa")
const auto s = fsv::filtered_string_view{str};
auto it = s.cend();
std::cout << *std::prev(it) << *std::prev(it, 2);
```
Output: `as`


### 2.10. Range

You are required to make a `filtered_string_view` be able to be used as a bidirectional range. This means that the standard `begin()`, `end()`, `cbegin()`, `cend()`, `rbegin()`, `rend()`, `crbegin()`, `crend()` suite of functions need to be implemented as member functions.

Furthermore, the bidirectional range type members:
- `iterator`
- `const_iterator`
- `reverse_iterator`
- `const_reverse_iterator`

should be publically available.

We have intentionally left off the return types for the the below functions. You will need to work out the correct number, const-correctness, and return types to ensure that all of the necessary overloads are present with the correct signatures.

**Note**: when we say "iterator" below, it could refer to any one of the four above iterator type aliases.

```cpp
auto begin();
auto cbegin();
```
Returns an iterator pointing to the start of the filtered string.

```cpp
auto end();
auto cend();
```

Returns an iterator pointing one-past-the-end of the filtered string, acting as a placeholder indicating there are no more characters to read.

```cpp
auto rbegin();
auto crbegin();
```

Returns an iterator pointing to the start of the reversed filtered string.

```cpp
auto rend();
auto crend();
```

Returns an iterator pointing one-past-the-end of the reversed filtered string, acting as a placeholder indicating there are no more characters to read.

**Hint**: You should leverage the standard library to create the reverse iterators.

#### Examples
```cpp
const auto s = fsv::filtered_string_view{"puppy", [](const char &c){ return !(c == 'u' || c == 'y'); }};
auto v = std::vector<char>{s.begin(), s.end()};
std::cout << v[0] << v[1] << v[2] << std::endl;
```
Output: `ppp`


```cpp
auto s = fsv::filtered_string_view{"milo", [](const char &c){ return !(c == 'i' || c == 'o'); }};
auto v = std::vector<char>{s.rbegin(), s.rend()};
std::cout << v[0] << v[1] << std::endl;
```
Output: `lm`

----


### 2.11. `const` Correctness
You must make sure that all appropriate member functions and member operators are `const`-qualified.

You must make sure that each member function and member operator appropriately either has:
1. A const qualified version only, or
2. A non-const qualified version only, or
3. Both a const and a non-const qualified version.

Please think carefully about this. The function declarations intentionally do not specify their
constness. Remember also that functions with both `const` and `non const` overloads may have different return types.

### 2.12. Exception Safety

You must make sure that all functions are `noexcept`-qualified where appropriate.

Please think carefully about what it means for a function to be `noexcept`. It is not enough that the function itself does not throw an exception -- any functions called in the implementation of that function must themselves be `noexcept`.

Functions that allocate memory are never `noexcept` (why?)

### 2.13. Performance

You must make sure your code does not perform its operations in a grossly inefficient manner.

When testing, we will put a one second time limit on each test.

### 2.14. Other Notes
You must:

- Ensure there is a header guard in `filtered_string_view.h`. It should already be there for you.
- Use C++20 style and methods where appropriate. This includes:
  - Using member-initialiser lists for constructors
  - Not misusing `explicit`
  - Following `const`-correctness
  - Following exception safety
- Implement all code within the `fsv` namespace.
- Use an appropriate STL algorithm instead of C style for loops wherever possible.

You must not:
- Use any existing `string_view` related utilities. This includes `std::string_view`.
- Use `std::ranges::views::filter_view`.
- Write to any files that aren't provided in the repo.
- Add a main function to `filtered_string_view.cpp`.
- Mutate the underlying string data in any way.
- Store a copy of the original or filtered string (remember that a `string_view` is supposed to be lightweight, non owning container).

You may assume:
- All inputs for all functions are valid. You do not need to do any verification of arguments.


## 3.Marking Criteria

This assignment will contribute 20% to your final mark.

The assessment for the assignment recognises the difficulty of the task, the importance of style,
and the importance of appropriate use of programming methods (e.g. using while loops instead of a
dozen if statements).

<table class="table table-bordered table-striped">
  <tr>
    <td align=right>50%</td>
    <td>
      <b>Correctness</b><br />
      The correctness of your program will be determined automatically by tests that we will run against
      your program. You will not know the full sample of tests used prior to marking.
    </td>
  </tr>
  <tr>
    <td align=right>25%</td>
    <td>
      <b>Your tests</b><br />
      You are required to write your own tests to ensure your program works.
      You will write tests in the <code>filtered_string_view.test.cpp</code> file. Please read the <a href="https://github.com/catchorg/Catch2/blob/master/docs/tutorial.md">Catch2 tutorial</a> or review lecture/lab content to see how to write tests. Tests will be marked on several
      factors. These include, <em>but are not limited to</em>:
      <ul>
        <li>Correctness — an incorrect test is worse than useless.</li>
        <li>
          Coverage - your tests might be great, but if they don't cover the part that ends up
          failing, they weren't much good to you.
        </li>
        <li>
          Brittleness — If you change your implementation, will the tests need to be changed (this
          generally means avoiding calling functions specific to your implementation where possible
          - ones that would be private if you were doing OOP).
        </li>
        <li>
          Clarity — If your test case failed, it should be immediately obvious what went wrong (this means splitting it up into appropriately sized sub-tests, amongst other things). Your tests should be well documented and clear in terms of the what the purpose of each of your tests is. I.E. What is the expected behaviour or edge case it is specifically testing. Where appropriate, an elaboration on *why* you're testing something is just as important as describing *what* you're testing.
        </li>
        <li>
          Simplicity — Your test cases should be simple, avoiding things like loops and other complex conditional
          logic so that the likelihood of of errors being introduced in your tests is reduced.
        </li>
      </ul>
      At least half of the marks of this section may be awarded with the expectation that your own tests pass your own code.
    </td>
  </tr>
  <tr>
    <td align=right>25%</td>
    <td>
      <b>C++ Style & Best Practices</b><br />
      Your adherence to good C++ best practices as shown in lectures and as documented on the coure website.
      This is based on how well you use modern C++ methodologies taught in this course as opposed to using
      backwards-compatible C methods.
      This section will also include marks for how well you follow general best porgramming practices, including maintaining
      small functions, reducing nesting in loops, having meaningful variable names, etc.
      You may also receive a penalty for this section if your code is not formatted perfectly according to the clang-format
      config file in your repo.
    </td>
  </tr>
</table>

Please note: Significant penalties may apply if you do not comply with the 'Git Commit Requirements' requirements as described in section 4 below.

## 4. Git Commit Requirements

It's imperative that we are able to track your progress when marking.

For assignment 1, there are some requirements for us to track your ongoing progress:

1. You must make commits on at least 3 unique days prior to due date.
2. All of your commits to master must successfully compile (according to the pipeline). You are given 3 exceptions.
3. Your commits must be meaningful in description (e.g. "Continued work on loop speed")
4. Each commit include no more than 50 lines additions of code (this may differ in future assignments). You are given no exceptions.

Failure to adhere to these guidelines in their entirety may result in a mimumum 20% penalty. Any moderate or significant failure may result in a 0 grade.

Please note: If you choose to work on separate branches before merging into master, you must squash your commits when merging back in. This means that you can make many commits on other branches fine, it's just whatever comes back to master needs to be a single commit that compiles with no more than 50 line additions.

## 5. Originality of Work

The work you submit must be your own work.  Submission of work partially or completely derived from
any other person or jointly written with any other person is not permitted.

The penalties for such an offence may include negative marks, automatic failure of the course and
possibly other academic discipline. Assignment submissions will be examined both automatically and
manually for such submissions.

Relevant scholarship authorities will be informed if students holding scholarships are involved in
an incident of plagiarism or other misconduct.

Do not provide or show your assignment work to any other person &mdash; apart from the teaching
staff of COMP6080.

If you knowingly provide or show your assignment work to another person for any reason, and work
derived from it is submitted, you may be penalised, even if the work was submitted without your
knowledge or consent.  This may apply even if your work is submitted by a third party unknown to
you.

Every time you make commits or pushes on this repository, you are acknowledging that the work you
submit is your own work (as described above).

Note you will not be penalised if your work has the potential to be taken without your consent or
knowledge.

<b>PLEASE NOTE: We have a record of ALL previous submissions of this assignment submitted. If you find a solution from a friend, or online, we will find it and you will receive 0 for the assignment and potentially 0 for the course.</b> Trust me, at least 1 person does it every term and I encourage you not to think you'll get lucky.

## 6. Dryrun

If you want to check if you've actually not totally screwed it all up, and see if they pass a very basic compilation example, then run this command on the CSE systems.

`6771 dryrun ass2`

## 7. Submission

This assignment is due *Friday 12th of July, 19:59:59*.

To submit your assignment, you must you've pushed all of your code to your gitlab master branch. You can check if you've done this properly by seeing what code is on the gitlab site on your master branch.
 
We will collect the latest work on your master branch of gitlab at the time of submission.

It is your responsibiltiy to ensure that your code can run successfully on a CSE machine / VLAB when cloned fresh from Gitlab.

## 8. Late Submission Policy

Late submissions are not accepted, except in the case of ELS adjustment or Special Consideration approval.
