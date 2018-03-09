#pragma once

#include <iterator>
#include <type_traits>

namespace detail_canTemplateApply
{
    template<typename T, template<typename> typename TT, typename = void>
    struct CanTemplateApply
        : std::false_type
    {};

    template<typename T, template<typename> typename TT>
    struct CanTemplateApply<T, TT, std::void_t<TT<T>>>
        : std::true_type
    {};
};

template<typename T, template<typename> typename TT>
constexpr bool canTemplateApply = detail_canTemplateApply::CanTemplateApply<T, TT>::value;


namespace detail_conditionalTemplateApply
{
    template<bool, template<typename...> typename TT_l, template<typename...> typename TT_r, typename... T>
    struct ConditionalTemplateApply
    {
        using type = TT_l<T...>;
    };

    template<template<typename...> typename TT_l, template<typename...> typename TT_r, typename... T>
    struct ConditionalTemplateApply<false, TT_l, TT_r, T...>
    {
        using type = TT_r<T...>;
    };
};

template<bool b, template<typename...> typename TT_l, template<typename...> typename TT_r, typename... T>
using ConditionalTemplateApply = typename detail_conditionalTemplateApply::ConditionalTemplateApply<b, TT_l, TT_r, T...>::type;


template<typename It>
using IteratorValueType = typename std::iterator_traits<It>::value_type;

template<typename It>
constexpr bool isIterator = canTemplateApply<It, IteratorValueType>;

template<typename It, typename Tag>
constexpr bool isTagIterator = isIterator<It> && std::is_base_of_v<Tag, typename std::iterator_traits<It>::iterator_category>;

template<typename It>
constexpr bool isInputIterator = isTagIterator<It, std::input_iterator_tag>;

template<typename It>
constexpr bool isForwardIterator = isTagIterator<It, std::forward_iterator_tag>;

template<typename It>
constexpr bool isBidirectionalIterator = isTagIterator<It, std::bidirectional_iterator_tag>;

template<typename It>
constexpr bool isRandomAccessIterator = isTagIterator<It, std::random_access_iterator_tag>;


namespace detail_valueType
{
    template<typename T>
    using DirectValueType = typename T::value_type;

    template<typename T>
    struct ValueType
    {
        using type = ConditionalTemplateApply<canTemplateApply<T, IteratorValueType>, IteratorValueType, DirectValueType, T>;
    };
};

template<typename T>
using ValueType = typename detail_valueType::ValueType<T>::type;
