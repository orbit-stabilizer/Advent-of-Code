from dataclasses import dataclass
from math import prod
from pathlib import Path


@dataclass
class Race:
  time: int
  distance: int

def main(text: str) -> int:
  races = parse_text(text)
  all_ways_to_win = [ways_to_win(race) for race in races]
  return prod(all_ways_to_win)

def parse_text(text: str) -> list[Race]:
  [(_, *times), (_, *distances)] = [line.split() for line in text.strip().split('\n')]
  times, distances = [int(time) for time in times], [int(distance) for distance in distances]
  return [Race(t, d) for t, d in zip(times, distances)]

def ways_to_win(race: Race) -> int:
  f = lambda t: -t ** 2 + race.time * t
  return sum(f(t) > race.distance for t in range(race.time))

if __name__ == '__main__':
  print(main(Path('sample.txt').read_text()))
  print(main(Path('z_input.txt').read_text()))
