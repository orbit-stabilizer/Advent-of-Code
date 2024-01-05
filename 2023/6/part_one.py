from dataclasses import dataclass
from functools import partial
from math import prod
from pathlib import Path


@dataclass
class Race:
  time: int
  distance: int

def f(t: int, t_0: int) -> int:
  return -t ** 2 + (t_0 * t)

def main(text: str) -> int:
  races = parse_text(text)
  all_ways_to_win = [ways_to_win(race) for race in races]
  return prod(all_ways_to_win)

def parse_text(text: str) -> list[Race]:
  times, distances = text.strip().split('\n')
  times, distances = times.split(), distances.split()
  (_, *times), (_, *distances) = times, distances
  times, distances = [int(time) for time in times], [int(distance) for distance in distances]
  races = zip(times, distances)
  races = [Race(t, d) for t, d in races]
  return races

def ways_to_win(race: Race) -> int:
  f_0 = partial(f, t_0=race.time)
  n = 0
  for t in range(race.time):
    y = f_0(t)
    if y > race.distance:
      n += 1
  return n

if __name__ == '__main__':
  print(main(Path('sample.txt').read_text()))
  print(main(Path('z_input.txt').read_text()))