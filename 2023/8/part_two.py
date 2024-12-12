import itertools
from pathlib import Path
from collections.abc import Iterator


type Network = dict[str, tuple[str, str]]
  
class Map:
  def __init__(self, document: str) -> None:
    raw_directions, _, *raw_network = document.strip().split('\n')
    self.cycle_len = len(raw_directions.strip()) 
    self.directions = self.build_directions(raw_directions)
    self.network = self.build_network(raw_network)

  def build_directions(self, raw_directions: str) -> Iterator[int]:
    """
    Builds an infinite repeating binary representation of the directions.

    @Role: Parse data to build data structure.

    Input -> Output
    ---------------
    'LR'  -> (0, 1, 0, 1, 0, 1, ...)
    'RRL' -> (1, 1, 0, 1, 1, 0, ...)

    Input
    -----
    raw_directions:
      A string of arbitrary size containing only two characters: 'L' and 'R'.
      Examples:
        - 'RL'
        - 'LRRRLL'
        - 'LRLRLRLRLRRLRLRLR'

    Output
    ------
    Iterator[int]:
      An infinite iterator containing only two integers: 0 and 1.
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

    @Role: Parse data to build data structure.

    Input -> Output
    ---------------
    ['AAA = (BBB, CCC)', 'BBB = (CCC, AAA)', 'CCC = (ZZZ, ZZZ)']
    ->
    {'AAA': ('BBB', 'CCC'), 'BBB': ('CCC', 'AAA'), 'CCC': ('ZZZ', 'ZZZ')}

    Input
    -----
    raw_network:
      A list of strings that contain the network information

    Output
    ------
    Network:
      A dict[str, tuple[str, str]] containing the same information as raw_network,
      just parsed into a dict.
    """
    network: Network = {}
    for line in raw_network:
      key, raw_values = [x.strip() for x in line.split('=')]
      first, second = [x.strip() for x in raw_values[1:-1].split(',')]
      network[key] = (first, second)
    return network
  
  def parallel_navigator(self) -> Iterator[list[str]]:
    """
    Generator function that returns the next list of nodes
    to navigate to. Skips yielding the starting nodes (the
    ones ending with 'A').
    """
    nodes = [key for key in self.network if key.endswith('A')]
    for direction in self.directions:
      nodes_to_yield = [self.network[node][direction] for node in nodes]
      yield nodes_to_yield
      nodes, nodes_to_yield = nodes_to_yield, []

  def find_steps_required(self) -> int:
    """
    Navigates self.network using self.parallel_navigator and finds number 
    of steps required to reach _Z starting from _A.
    """
    steps = 1
    for nodes in self.parallel_navigator():
      if all(node.endswith('Z') for node in nodes): break
      steps += 1
    return steps

    #threads = range(len(nodes))
    #record: dict[int, dict[int, set[str]]] = {thread: {n: set() for n in range(self.cycle_len)} for thread in threads}
    #locations = {thread: 0 for thread in threads}
    #for thread in threads:
      #node, n, step = nodes[thread], 0, 0
      #try:
        #for direction in self.directions:
          #if node in record[thread][n]: raise ValueError
          #record[thread][n].add(node)
          #node, n = self.network[node][direction], (n + 1) % self.cycle_len
          #step += 1
      #except ValueError:
        #print('hiiii')
        #print(thread, node)
        #locations[thread] = step
#
    #max_location = max(locations.values())
    #thread_nodes = {thread: '' for thread in threads}
    #for thread in threads:
      #diff = max_location - locations[thread]
      #n = locations[thread]
      #for _ in range(diff):
        #node, n = self.network[node][direction], (n + 1)
      #thread_nodes[thread] = node
      #locations[thread] = n
#
    #print(locations[thread] % self.cycle_len)
    #distances = {thread: set() for thread in threads}
    #for thread in threads:
      #step = 0
      #node = thread_nodes[thread]
      #try:
        #for direction in self.directions:
          ##print(node)
          #step += 1
          #node = self.network[node][direction]
          #if node == thread_nodes[thread] and step % self.cycle_len == 2: raise ValueError
          #if node.endswith('Z'): distances[thread].add(step)
      #except ValueError:
        #print(f'step {thread}:', step)
#
    #print(distances)
    #return thread_nodes
  

def main(document: str) -> int:
  map = Map(document)
  return map.find_steps_required()

if __name__ == '__main__':
    #print(main(Path('sample_3.txt').read_text()))
  print(main(Path('z_input.txt').read_text()))
