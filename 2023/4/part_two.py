from collections import defaultdict
from pathlib import Path


class Deck:
  def __init__(self, text: str) -> None:
    self.lines = self.process_text(text)
    self.cards = self.build_cards()
    self.compute_cards_total()

  def process_text(self, text: str) -> list[str]:
    return text.strip().split('\n')
  
  def build_cards(self) -> defaultdict[int, int]:
    cards = defaultdict(int)
    for n, _ in enumerate(self.lines, start=1):
      cards[n] = 1
    return cards

  def compute_cards_total(self) -> None:
    for card, line in enumerate(self.lines, start=1):
      self.compute_card_total(card, line)
  
  def compute_card_total(self, current_card: int, line: str) -> None:
    _, numbers = line.split(':')
    winning_numbers, our_numbers = numbers.split('|')
    winning_numbers, our_numbers = set(winning_numbers.strip().split()), set(our_numbers.strip().split())
    n = len(winning_numbers.intersection(our_numbers))
    for card in range(current_card + 1, current_card + n + 1):
      self.cards[card] += self.cards[current_card]
  
  def get_total_scratchcards(self) -> int:
    return sum(self.cards.values())


def main(text: str) -> int:
  deck = Deck(text)
  return deck.get_total_scratchcards()

if __name__ == '__main__':
  print(main(Path('sample.txt').read_text()))
  print(main(Path('z_input.txt').read_text()))