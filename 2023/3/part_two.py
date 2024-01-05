from pathlib import Path

from part_one import Grid


def main(text: str) -> int:
  grid = Grid(text)
  return compute_gear_ratio_sum(grid)

def compute_gear_ratio_sum(grid: Grid) -> int:
  gear_ratio_sum = 0
  for row, xs in grid.symbols.items():
    rows_ = [row - 1, row, row + 1]
    for x in xs:
      gear_ratio = 1
      adjacent = 0
      for row_ in rows_:
        for number in grid.numbers[row_]:
          if x in number.xs:
            adjacent += 1
            gear_ratio *= number.n
      if adjacent == 2:
        gear_ratio_sum += gear_ratio
  return gear_ratio_sum

if __name__ == '__main__':
  print(main(Path('sample.txt').read_text()))
  print(main(Path('z_input.txt').read_text()))