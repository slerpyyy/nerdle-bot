use std::ops::Range;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValueSet {
    range: Range<i32>,
}

impl ValueSet {
    pub fn from_range(range: Range<i32>) -> Self {
        Self { range }
    }

    pub fn range(&self) -> Range<i32> {
        self.range.clone()
    }

    pub fn intersection(&self, other: &Self) -> Self {
        let min = self.range.start.max(other.range.start);
        let max = self.range.end.min(other.range.end);

        Self { range: min..max }
    }

    pub fn union(&self, other: &Self) -> Self {
        let min = self.range.start.min(other.range.start);
        let max = self.range.end.max(other.range.end);

        Self { range: min..max }
    }

    pub fn is_empty(&self) -> bool {
        self.range.is_empty()
    }
}

impl std::ops::Neg for ValueSet {
    type Output = Self;

    fn neg(self) -> Self::Output {
        let min = -self.range.end;
        let max = -self.range.start;

        Self { range: min..max }
    }
}

impl std::ops::Add for ValueSet {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        let min = self.range.start + rhs.range.start;
        let max = self.range.end + rhs.range.end;
        Self { range: min..max }
    }
}

impl std::ops::Sub for ValueSet {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        let min = self.range.start - rhs.range.end;
        let max = self.range.end - rhs.range.start;
        Self { range: min..max }
    }
}

impl std::ops::Mul for ValueSet {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        let corners = [
            self.range.start * rhs.range.start,
            self.range.start * rhs.range.end,
            self.range.end * rhs.range.start,
            self.range.end * rhs.range.end,
        ];

        let min = *corners.iter().min().unwrap();
        let max = *corners.iter().max().unwrap();
        Self { range: min..max }
    }
}

impl std::ops::Div for ValueSet {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        let corners = [
            self.range.start.checked_div(rhs.range.start),
            self.range.end.checked_div(rhs.range.start),
            self.range.start.checked_div(rhs.range.end),
            self.range.end.checked_div(rhs.range.end),
            rhs.range.contains(&1).then(|| self.range.start),
            rhs.range.contains(&1).then(|| self.range.end),
            rhs.range.contains(&-1).then(|| -self.range.start),
            rhs.range.contains(&-1).then(|| -self.range.end),
        ];

        let min = *corners.iter().flatten().min().unwrap();
        let max = *corners.iter().flatten().max().unwrap();
        Self { range: min..max }
    }
}
