import math
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path


@dataclass
class Number:
  n: int
  xs: range

class Grid:
  def __init__(self, text: str) -> None:
    rows = text.strip().split('\n')
    self.numbers: dict[int, list[Number]] = defaultdict(list)
    self.symbols: dict[int, list[int]] = defaultdict(list)
    self.populate_numbers(rows)
    self.populate_symbols(rows)
  
  def populate_numbers(self, rows: list[str]) -> None:
    n, start = 0, math.inf
    for i, row in enumerate(rows):
      if n:
        self.numbers[i - 1].append(Number(n, range(start - 1, j + 1)))
        n, start = 0, math.inf
      for j, value in enumerate(row):
        if value.isdigit():
          start = min(start, j)
          n *= 10
          n += int(value)
          continue
        if n:
          self.numbers[i].append(Number(n, range(start - 1, j + 1)))
          n, start = 0, math.inf

  def populate_symbols(self, rows: list[str]) -> None:
    for i, row in enumerate(rows):
      for j, value in enumerate(row):
        if not value.isdigit() and value != '.':
          self.symbols[i].append(j)


def main(text: str) -> int:
  grid = Grid(text)
  return compute_total(grid)

def compute_total(grid: Grid) -> int:
  total = 0
  for row, numbers in grid.numbers.items():
    for number in numbers:
      try:
        rows_ = [row - 1, row, row + 1]
        for row_ in rows_:
          for x in grid.symbols[row_]:
            if x in number.xs:
              total += number.n
              raise StopIteration
      except StopIteration:
        pass
  return total

if __name__ == '__main__':
  print(main(Path('sample.txt').read_text()))
  print(main(Path('z_input.txt').read_text()))