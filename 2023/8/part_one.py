import itertools
from collections.abc import Iterator
from pathlib import Path


type Network = dict[str, tuple[str, str]]
  
class Map:
  def __init__(self, document: str) -> None:
    raw_directions, _, *raw_network = document.strip().split('\n')
    self.directions = self.build_directions(raw_directions)
    self.network = self.build_network(raw_network)

  def build_directions(self, raw_directions: str) -> Iterator[int]:
    """
    Builds an infinite repeating binary representation of the directions.

    Input -> Output
    ---------------
    'LR'  -> (0, 1, 0, 1, 0, 1, ...)
    'RRL' -> (1, 1, 0, 1, 1, 0, ...)

    Input
    -----
    raw_directions:
      A string of arbitrary size containing only two characters: 'L' and 'R',
      which give the directions one can go, left or right.
    Examples:
      - 'RL'
      - 'LRRRLL'
      - 'LRLRLRLRLRRLRLRLR'

    Output
    ------
    Iterator[int]:
      A stream of 0s and 1s.
    Examples:
      - (0, 1, 0, 1, 0, 1, ...)
      - (1, 1, 0, 1, 1, 0, ...)
      - (0, 0, 1, 0, 0, 1, ...)
    """
    to_bin = {'L': 0, 'R': 1}
    return itertools.cycle([to_bin[c] for c in raw_directions])

  def build_network(self, raw_network: list[str]) -> Network:
    """
    Builds a network graph where the keys are the current nodes and the
    values are 2-tuples that contain the two nodes that are accessible
    from the current node by going left or right. 

    Input -> Output
    ---------------
    ['AAA = (BBB, CCC)', 'BBB = (CCC, AAA)', 'CCC = (ZZZ, ZZZ)']
    ->
    {'AAA': ('BBB', 'CCC'), 'BBB': ('CCC', 'AAA'), 'CCC': ('ZZZ', 'ZZZ')}

    Input
    -----
    raw_network:
      A list of strings that contain the network information.

    Output
    ------
    Network:
      A dict version of raw_network.
    """
    network: Network = {}
    for line in raw_network:
      key, raw_values = [x.strip() for x in line.split('=')]
      first, second = [x.strip() for x in raw_values[1:-1].split(',')]
      network[key] = (first, second)
    return network

  def find_steps_required(self) -> int:
    """
    Navigates self.network using self.directions and finds number of steps 
    required to reach ZZZ starting from AAA.
    """
    node, steps, destination = 'AAA', 0, 'ZZZ'
    for direction in self.directions:
      steps += 1
      node = self.network[node][direction]
      if node == destination: break
    return steps


def main(document: str) -> int:
  map = Map(document)
  return map.find_steps_required()

if __name__ == '__main__':
  print(main(Path('sample.txt').read_text()))
  print(main(Path('sample_2.txt').read_text()))
  print(main(Path('z_input.txt').read_text()))
