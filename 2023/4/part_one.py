from pathlib import Path

def main(text: str) -> int:
  list_of_points = [compute_points(line) for line in text.strip().split('\n')]
  return sum(list_of_points)

def compute_points(line: str) -> int:
  _, numbers = line.split(':')
  winning_numbers, our_numbers = numbers.split('|')
  winning_numbers, our_numbers = set(winning_numbers.strip().split()), set(our_numbers.strip().split())
  matching_numbers = len(winning_numbers.intersection(our_numbers))
  return 2 ** (matching_numbers - 1) if matching_numbers else 0

if __name__ == '__main__':
  print(main(Path('sample.txt').read_text()))
  print(main(Path('z_input.txt').read_text()))