#![cfg(test)]
use std::collections::HashSet;

use super::*;
use proptest::prelude::*;
use test_case::test_case;

#[test_case(vec![(2, 'a'), (2, 'b'), (3, 'c'), (1, 'd'), (3, 'e')], vec![(1, 'd'), (2, 'a'), (2, 'b'), (3, 'c'), (3, 'e')])]
fn test_sort_iterator_adapter_manually(unsorted: Vec<(u8, char)>, expected: Vec<(u8, char)>) {
    let sorted = unsorted.into_iter().sort(|(i, _)| *i);

    println!("{:?}", sorted);
    assert_eq!(sorted.collect::<Vec<_>>(), expected);
}

prop_compose! {
    fn arb_key_value(n_keys: u8)(key in 0..n_keys, value in any::<i32>()) -> (u8, i32) {
         (key, value)
    }
}

proptest! {
    #[test]
    fn test_sort_iterator_adapter(unsorted in proptest::collection::vec(arb_key_value(10), 0..100)) {
        let sorted = unsorted.iter().sort(|(i, _)| *i).map(|(k, v)| (*k, *v)).collect::<Vec<_>>();

        assert_eq!(sorted.len(), unsorted.len());

        let keys = unsorted.iter().map(|(k, _v)| *k).collect::<HashSet<_>>();
        for key in keys {
            let matches_key = |(k, v): &(u8, i32)| (*k == key).then_some(*v);
            let unsorted_values = unsorted.iter().filter_map(matches_key).collect::<Vec<_>>();
            let sorted_values = sorted.iter().filter_map(matches_key).collect::<Vec<_>>();

            assert_eq!(&sorted_values, &unsorted_values);
        }
    }
}
