from pathlib import Path


def main(text: str) -> int:
  input = parse(text)
  return sum(predict_line(line) for line in input)

def parse(text: str) -> list[list[int]]:
  return [[int(val) for val in line.split()] for line in text.splitlines()]

def predict_line(line: list[int]) -> int:
  diffs = [lead - lag for lead, lag in zip(line[1:], line)]
  if sum(line) == 0: return 0 
  return line[-1] + predict_line(diffs)

if __name__ == '__main__':
    print(main(Path('document.txt').read_text()))
