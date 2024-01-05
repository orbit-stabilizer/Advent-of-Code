from pathlib import Path


class Almanac:
  def __init__(self, text: str) -> None:
    self.seeds = []
    self.seed_locations = []
    self.seed_to_soil = {}
    self.soil_to_fertilizer = {}
    self.fertilizer_to_water = {}
    self.water_to_light = {}
    self.light_to_temperature = {}
    self.temperature_to_humidity = {}
    self.humidity_to_location = {}
    self.maps = {
      'seed-to-soil map:': self.seed_to_soil,
      'soil-to-fertilizer map:': self.soil_to_fertilizer,
      'fertilizer-to-water map:': self.fertilizer_to_water,
      'water-to-light map:': self.water_to_light,
      'light-to-temperature map:': self.light_to_temperature,
      'temperature-to-humidity map:': self.temperature_to_humidity,
      'humidity-to-location map:': self.humidity_to_location,
    }
    self.populate_data(text)
    self.populate_seed_locations()
  
  def populate_data(self, text: str) -> None:
    lines = text.strip().split('\n')
    seeds_line, _, *lines = lines + ['']
    self.populate_seeds(seeds_line)

    map_name, mappings = '', []
    for line in lines:
      if 'map' in line:
        map_name = line
      elif not line:
        self.populate_map(map_name, mappings)
        mappings = []
      else:
        mappings.append(line)

  def populate_seeds(self, line: str) -> None:
    _, seeds = line.split(':')
    seeds = seeds.strip().split()
    self.seeds = [int(seed) for seed in seeds]
  
  def populate_map(self, map_name: str, mappings: list[str]) -> None:
    map = self.maps[map_name]
    split_mapping_data = lambda mapping: [int(value) for value in mapping.split()]
    for mapping in mappings:
      destination_range_start, source_range_start, range_length = split_mapping_data(mapping)
      diff = destination_range_start - source_range_start
      map[range(source_range_start, source_range_start + range_length)] = diff

  def populate_seed_locations(self) -> None:
    self.seed_locations = [self.get_seed_location(seed) for seed in self.seeds]

  def get_seed_location(self, seed: int) -> int:
    soil = self.find_map_value('seed-to-soil map:', seed)
    fertilizer = self.find_map_value('soil-to-fertilizer map:', soil)
    water = self.find_map_value('fertilizer-to-water map:', fertilizer)
    light = self.find_map_value('water-to-light map:', water)
    temperature = self.find_map_value('light-to-temperature map:', light)
    humidity = self.find_map_value('temperature-to-humidity map:', temperature)
    location = self.find_map_value('humidity-to-location map:', humidity)
    return location
  
  def find_map_value(self, map_name: str, source: int) -> int:
    map = self.maps[map_name]
    for k, v in map.items():
      if source in k: return source + v
    return source

  def get_min_seed_location(self) -> int:
    return min(self.seed_locations)

def main(text: str) -> int:
  alamanc = Almanac(text)
  return alamanc.get_min_seed_location()

if __name__ == '__main__':
  print(main(Path('sample.txt').read_text()))
  print(main(Path('z_input.txt').read_text()))