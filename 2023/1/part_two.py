from pathlib import Path

from part_one import get_calibration_value


def parse_document(document: str) -> int:
    return sum(get_calibration_value(convert_line(line)) for line in document.strip().split('\n'))

def convert_line(line: str) -> str:
  d = {}
  for idx, _ in enumerate(line):
    if line[idx:idx + 3] == 'one':
      d[idx] = '1'
    elif line[idx:idx + 3] == 'two':
      d[idx] = '2'
    elif line[idx:idx + 5] == 'three':
      d[idx] = '3'
    elif line[idx:idx + 4] == 'four':
      d[idx] = '4'
    elif line[idx:idx + 4] == 'five':
      d[idx] = '5'
    elif line[idx:idx + 3] == 'six':
      d[idx] = '6'
    elif line[idx:idx + 5] == 'seven':
      d[idx] = '7'
    elif line[idx:idx + 5] == 'eight':
      d[idx] = '8'
    elif line[idx:idx + 4] == 'nine':
      d[idx] = '9'
  for n, (pos, digit) in enumerate(d.items()):
    line = line[:pos + n] + digit + line[pos + n:]
  return line

if __name__ == '__main__':
  print(parse_document(Path('sample_input_2.txt').read_text()))
  print(parse_document(Path('document.txt').read_text()))
