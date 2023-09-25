use std::collections::{BTreeMap, VecDeque};

/// See [sort].
pub struct Sort<K, V> {
    head_bucket: Option<VecDeque<V>>,
    tail_buckets: BTreeMap<K, VecDeque<V>>,
}

impl<K, V> Iterator for Sort<K, V>
where
    K: Ord,
{
    type Item = V;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(head_bucket) = &mut self.head_bucket {
            head_bucket.pop_front()
        } else {
            self.head_bucket = self.tail_buckets.pop_first().map(|(_k, v)| v);
            if let Some(head_bucket) = &mut self.head_bucket {
                head_bucket.pop_front()
            } else {
                None
            }
        }
    }
}

impl<K, V> Sort<K, V> {
    pub fn new<I, F>(iter: I, keyfn: F) -> Self
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
