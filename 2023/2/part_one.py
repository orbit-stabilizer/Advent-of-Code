from dataclasses import dataclass
from pathlib import Path


MAX_RED = 12
MAX_GREEN = 13
MAX_BLUE = 14

@dataclass
class Game:
  game_id: int
  max_red_seen: int
  max_green_seen: int
  max_blue_seen: int

def main(text: str) -> int:
  parsed_text = parse_text(text)
  games = [build_game(line) for line in parsed_text]
  invalid_games = [find_valid_game(game) for game in games]
  return sum(invalid_games)

def parse_text(text: str) -> list[str]:
  return text.strip().split('\n')

def build_game(line: str) -> Game:
  line = line.replace('; ', ', ')
  game_id = get_game_id(line)
  max_colours_seen = get_max_colours_seen(line)
  game = Game(game_id, max_colours_seen['red'], max_colours_seen['green'], max_colours_seen['blue'])
  return game

def get_game_id(line: str) -> int:
  game_id_section, *_  = line.split(':')
  _, game_id = game_id_section.split()
  return int(game_id)

def get_max_colours_seen(line: str) -> dict[str, int]:
  max_colours_seen = {
    'red': 0,
    'green': 0,
    'blue': 0,
  }
  _, colours_section = line.split(':')
  colours_and_counts = colours_section.split(',')
  for colour_and_count in colours_and_counts:
    count, colour = colour_and_count.split()
    max_colours_seen[colour] = max(max_colours_seen[colour], int(count))
  return max_colours_seen

def find_valid_game(game: Game) -> int:
  if game.max_red_seen <= MAX_RED and game.max_green_seen <= MAX_GREEN and game.max_blue_seen <= MAX_BLUE:
    return game.game_id
  else:
    return 0

if __name__ == '__main__':
  print(main(Path('sample.txt').read_text()))
  print(main(Path('z_input.txt').read_text()))