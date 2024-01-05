from pathlib import Path

from part_one import build_game, Game, parse_text


def main(text: str) -> int:
  parsed_text = parse_text(text)
  games = [build_game(line) for line in parsed_text]
  powers_of_games = [find_power_of_game(game) for game in games]
  return sum(powers_of_games)

def find_power_of_game(game: Game) -> int:
  return game.max_red_seen * game.max_green_seen * game.max_blue_seen

if __name__ == '__main__':
  print(main(Path('sample.txt').read_text()))
  print(main(Path('z_input.txt').read_text()))