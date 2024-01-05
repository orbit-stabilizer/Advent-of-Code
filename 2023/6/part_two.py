from dataclasses import dataclass
from math import ceil, floor, sqrt
from pathlib import Path


@dataclass
class Race:
  time: int
  distance: int

def main(text: str) -> int:
  race = parse_text(text)
  return ways_to_win(race)

def parse_text(text: str) -> Race:
  times, distances = text.strip().split('\n')
  times, distances = times.split(), distances.split()
  (_, *times), (_, *distances) = times, distances
  time, distance = ''.join(times), ''.join(distances)
  time, distance = int(time), int(distance)
  race = Race(time, distance)
  return race

def ways_to_win(race: Race) -> int:
  t, d = race.time, race.distance
  # Use quadratic forumula
  x_1, x_2 = (t - sqrt(t ** 2 - (4 * d)) / 2), (t + sqrt(t ** 2 - (4 * d)) / 2)
  x_1, x_2 = floor(x_1) + 1, ceil(x_2) - 1
  return x_2 - x_1 + 1

if __name__ == '__main__':
  print(main(Path('sample.txt').read_text()))
  print(main(Path('z_input.txt').read_text()))