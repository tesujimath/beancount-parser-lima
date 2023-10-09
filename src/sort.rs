use std::{
    collections::{BTreeMap, VecDeque},
    fmt::{self, Debug},
};

/// See [sort].
pub struct Sort<K, V> {
    head_bucket: Option<VecDeque<V>>,
    tail_buckets: BTreeMap<K, VecDeque<V>>,
}

impl<K, V> Sort<K, V> {
    fn new<I, F>(iter: I, keyfn: F) -> Self
    where
        K: Ord + Copy,
        I: Iterator<Item = V>,
        F: Fn(&V) -> K,
    {
        let mut buckets = BTreeMap::new();

        for item in iter {
            let key = keyfn(&item);

            match buckets.get_mut(&key) {
                None => {
                    buckets.insert(key, VecDeque::new());
                    let bucket = buckets.get_mut(&key).unwrap();
                    bucket.push_back(item);
                }
                Some(bucket) => {
                    bucket.push_back(item);
                }
            }
        }

        Sort {
            head_bucket: None,
            tail_buckets: buckets,
        }
    }
}

impl<K, V> Iterator for Sort<K, V>
where
    K: Ord,
{
    type Item = V;

    fn next(&mut self) -> Option<Self::Item> {
        // do we need to get next bucket?
        if match &self.head_bucket {
            None => true,
            Some(b) => b.is_empty(),
        } {
            self.head_bucket = self.tail_buckets.pop_first().map(|(_k, v)| v);
        }

        self.head_bucket.as_mut().and_then(|b| b.pop_front())
    }
}

impl<K, V> Debug for Sort<K, V>
where
    K: Debug,
    V: Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Sort")
            .field("head_bucket", &self.head_bucket)
            .field("tail_bucket", &self.tail_buckets)
            .finish()
    }
}

pub trait SortIterator<K, V, F>: Iterator<Item = V> + Sized {
    /// Iterator adapter for stable sort.
    ///
    /// Items are returned in order of key, where items with the same key remain in their original order.
    fn sort(self, keyfn: F) -> Sort<K, V>
    where
        K: Ord + Copy,
        F: Fn(&V) -> K,
    {
        Sort::new(self, keyfn)
    }
}

impl<K, V, F, I: Iterator<Item = V>> SortIterator<K, V, F> for I {}

mod tests;
