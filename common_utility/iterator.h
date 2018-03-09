#pragma once

#include "type_traits.h"

#include <iterator>
#include <type_traits>
#include <utility>
#include <valarray>


template<typename Container, typename = void, typename = void>
constexpr decltype(auto) data(Container&& c) noexcept
{
    return std::data(std::forward<Container>(c));
}

template<typename ValArray, typename = std::enable_if_t<std::is_same_v<std::remove_cv_t<ValArray>, std::valarray<ValueType<ValArray>>>>>
constexpr decltype(auto) data(ValArray& c) noexcept
{
    return &c[0];
}
