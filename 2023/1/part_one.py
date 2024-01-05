from pathlib import Path
from typing import Callable


def parse_document(document: str) -> int:
  return sum(get_calibration_value(line) for line in document.strip().split('\n'))

def get_calibration_value(line: str) -> int:
  get_first_digit = lambda line: next(char for char in line if char.isdigit())
  first_digit, last_digit = get_first_digit(line), get_first_digit(line[::-1])
  return int(first_digit + last_digit)

if __name__ == '__main__':
  print(parse_document(Path('sample_input_1.txt').read_text()))
  print(parse_document(Path('document.txt').read_text()))
