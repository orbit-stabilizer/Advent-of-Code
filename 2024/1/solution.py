from pathlib import Path


def create_lists(text: str) -> tuple[list[int], list[int]]:
  lines = [line.split() for line in text.splitlines()]
  nums = [int(num) for line in lines for num in line]
  left_list, right_list = nums[::2], nums[1::2]
  return sorted(left_list), sorted(right_list)

def compute_distance(left_list: list[int], right_list: list[int]) -> int:
  return sum(abs(left - right) for left, right in zip(left_list, right_list))

def main(text: str) -> int:
  left_list, right_list = create_lists(text)
  return compute_distance(left_list, right_list)
      
if __name__ == '__main__':
  document = Path('input.txt').read_text()
  print(main(document))
