#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Interval {
    pub start: i32,
    pub end: i32,
}

impl std::fmt::Display for Interval {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}; {}]", self.start, self.end)
    }
}

impl Interval {
    pub fn new(start: i32, end: i32) -> Self {
        Self { start, end }
    }

    #[cfg(test)]
    pub fn empty() -> Self {
        Self { start: 1, end: 0 }
    }

    pub fn len(&self) -> i32 {
        self.end - self.start + 1
    }

    pub fn is_empty(&self) -> bool {
        self.end < self.start
    }

    #[cfg(test)]
    pub fn contains(&self, item: i32) -> bool {
        self.start <= item && item <= self.end
    }

    pub fn union(self, rhs: Self) -> Self {
        let min = std::cmp::min(self.start, rhs.start);
        let max = std::cmp::max(self.end, rhs.end);
        Self::new(min, max)
    }

    pub fn intersection(self, rhs: Self) -> Self {
        let min = std::cmp::max(self.start, rhs.start);
        let max = std::cmp::min(self.end, rhs.end);
        Self::new(min, max)
    }

    pub fn overlap(self, rhs: Self) -> i32 {
        self.intersection(rhs).len()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Domain {
    ranges: Vec<Interval>,
}

impl std::fmt::Display for Domain {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{ ")?;
        for inter in &self.ranges {
            write!(f, "{inter} ")?;
        }
        write!(f, "}}")
    }
}

impl Domain {
    pub fn new() -> Self {
        Self { ranges: Vec::new() }
    }

    pub fn from_range(range: Interval) -> Self {
        let mut this = Self::new();
        this.add_range(range);
        this
    }

    pub fn from_outer_union<F>(left: &[Interval], right: &[Interval], mut product: F) -> Self
    where
        F: FnMut(Interval, Interval) -> Interval,
    {
        let mut out = Self::new();
        for left in left {
            for right in right {
                let fragment = product(left.clone(), right.clone());
                out.add_range(fragment);
            }
        }

        out
    }

    pub fn is_empty(&self) -> bool {
        self.ranges.iter().all(|r| r.is_empty())
    }

    #[cfg(test)]
    pub fn contains(&self, item: i32) -> bool {
        self.ranges.iter().any(|r| r.contains(item))
    }

    #[cfg(test)]
    pub fn range(&self) -> Interval {
        self.ranges
            .iter()
            .cloned()
            .reduce(|a, b| a.union(b))
            .unwrap_or(Interval::empty())
    }

    pub fn add_range(&mut self, mut range: Interval) {
        if range.is_empty() {
            return;
        }

        while let Some(index) = self
            .ranges
            .iter()
            .position(|&other| other.overlap(range) >= 0)
        {
            let other = self.ranges.swap_remove(index);
            range = range.union(other);
        }

        self.ranges.push(range);
    }

    pub fn union(&self, other: &Self) -> Self {
        other
            .ranges
            .iter()
            .cloned()
            .fold(self.clone(), |mut acc, val| {
                acc.add_range(val);
                acc
            })
    }

    pub fn intersection(&self, other: &Self) -> Self {
        Self::from_outer_union(&self.ranges, &other.ranges, |a, b| a.intersection(b))
    }
}

impl std::ops::Neg for Domain {
    type Output = Self;

    fn neg(mut self) -> Self::Output {
        self.ranges.iter_mut().for_each(|range| {
            *range = Interval::new(-range.end, -range.start);
        });

        self
    }
}

impl std::ops::Add for Domain {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self::from_outer_union(&self.ranges, &rhs.ranges, |a, b| {
            let min = a.start + b.start;
            let max = a.end + b.end;
            Interval::new(min, max)
        })
    }
}

impl std::ops::Sub for Domain {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self::from_outer_union(&self.ranges, &rhs.ranges, |a, b| {
            let min = a.start - b.end;
            let max = a.end - b.start;
            Interval::new(min, max)
        })
    }
}

impl std::ops::Mul for Domain {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self::from_outer_union(&self.ranges, &rhs.ranges, |a, b| {
            let corners = [
                a.start * b.start,
                a.start * b.end,
                a.end * b.start,
                a.end * b.end,
            ];

            let min = *corners.iter().min().unwrap();
            let max = *corners.iter().max().unwrap();
            Interval::new(min, max)
        })
    }
}

impl std::ops::Div for Domain {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        let mut out = Self::new();

        let right_iter = rhs
            .ranges
            .into_iter()
            .flat_map(|full| {
                let neg = Interval::new(full.start, full.end.min(-1));
                let pos = Interval::new(full.start.max(1), full.end);
                [neg, pos]
            })
            .filter(|inter| !inter.is_empty());

        for right in right_iter {
            for left in &self.ranges {
                if right.len() == 1 && left.len() == 1 {
                    let right = right.start;
                    let left = left.start;

                    if left.checked_rem(right) == Some(0) {
                        let q = left / right;
                        out.add_range(Interval::new(q, q));
                    }

                    continue;
                }

                let corners = [
                    left.start.checked_div(right.start),
                    left.end.checked_div(right.start),
                    left.start.checked_div(right.end),
                    left.end.checked_div(right.end),
                ];

                let min = corners.iter().flatten().min();
                let max = corners.iter().flatten().max();

                if let (Some(&min), Some(&max)) = (min, max) {
                    let inter = Interval::new(min, max);
                    out.add_range(inter);
                }
            }
        }

        out
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn interval_overlapping() {
        let a = Interval::new(2, 5);
        let b = Interval::new(4, 7);

        assert_eq!(a.len(), 4);
        assert_eq!(b.len(), 4);

        assert_eq!(a.union(b), Interval::new(2, 7));
        assert_eq!(a.intersection(b), Interval::new(4, 5));

        assert_eq!(a.overlap(b), 2);
        assert_eq!(a.intersection(b).is_empty(), false);
    }

    #[test]
    fn interval_touching() {
        let a = Interval::new(2, 5);
        let b = Interval::new(6, 8);

        assert_eq!(a.len(), 4);
        assert_eq!(b.len(), 3);

        assert_eq!(a.union(b), Interval::new(2, 8));
        assert_eq!(a.intersection(b), Interval::new(6, 5));

        assert_eq!(a.overlap(b), 0);
        assert_eq!(a.intersection(b).is_empty(), true);
    }

    #[test]
    fn interval_far_apart() {
        let a = Interval::new(2, 5);
        let b = Interval::new(20, 23);

        assert_eq!(a.union(b), Interval::new(2, 23));
        assert_eq!(a.intersection(b), Interval::new(20, 5));

        assert_eq!(a.overlap(b), -14);
        assert_eq!(a.intersection(b).is_empty(), true);
    }

    #[test]
    fn range_chain() {
        let mut set = Domain::new();
        set.add_range(Interval::new(2, 5));
        set.add_range(Interval::new(7, 12));
        set.add_range(Interval::new(3, 9));

        assert_eq!(set.ranges, &[Interval::new(2, 12)]);
    }

    #[test]
    fn domain_div_simple() {
        let left = Domain::from_range(Interval::new(25, 50));
        let right = Domain::from_range(Interval::new(-5, 5));

        let result = left / right;
        println!("{result}");

        for x in 25..=50 {
            for y in -5..=5 {
                if y == 0 {
                    continue;
                }

                assert!(result.contains(x / y));
            }
        }
    }

    #[test]
    fn domain_div_sub_10() {
        const LIM: i32 = 10;

        let left = Domain::from_range(Interval::new(-LIM, LIM));
        let right = left.clone();

        let expected = left.clone();
        let result = left / right;

        assert_eq!(result, expected);
    }

    #[test]
    fn domain_div_by_zero() {
        let left = Domain::from_range(Interval::new(-2, 5));
        let right = Domain::from_range(Interval::new(0, 0));
        let result = left / right;

        assert!(result.is_empty());
    }
}
