from collections import Counter
from enum import Enum
from functools import total_ordering
from pathlib import Path
from typing import Self


class OrderedEnum(Enum):
    def __ge__(self, other: Self):
        if self.__class__ is other.__class__:
            return self.value >= other.value
        return NotImplemented
    def __gt__(self, other: Self):
        if self.__class__ is other.__class__:
            return self.value > other.value
        return NotImplemented
    def __le__(self, other: Self):
        if self.__class__ is other.__class__:
            return self.value <= other.value
        return NotImplemented
    def __lt__(self, other: Self):
        if self.__class__ is other.__class__:
            return self.value < other.value
        return NotImplemented

class HandType(OrderedEnum):
  HIGH_CARD = 1
  ONE_PAIR = 2 
  TWO_PAIR = 3
  THREE_OF_A_KIND = 4
  FULL_HOUSE = 5
  FOUR_OF_A_KIND = 6
  FIVE_OF_A_KIND = 7 

class Card(OrderedEnum):
  TWO = 1
  THREE = 2
  FOUR = 3
  FIVE = 4
  SIX = 5
  SEVEN = 6
  EIGHT = 7
  NINE = 8
  TEN = 9
  JACK = 10
  QUEEN = 11
  KING = 12
  ACE = 13


@total_ordering
class Hand:
  def __init__(self, hand: str, bid: str) -> None:
    self.bid = int(bid)
    self.hand = hand
    self.type = self.find_hand_type()
    self.cards = self.get_cards()

  def find_hand_type(self) -> HandType:
    unique_cards = len(set(self.hand))
    counts = Counter(self.hand)
    match unique_cards:
      case 5: return HandType.HIGH_CARD
      case 4: return HandType.ONE_PAIR
      case 3:
        if 2 in counts.values(): return HandType.TWO_PAIR
        if 3 in counts.values(): return HandType.THREE_OF_A_KIND
      case 2:
        if 3 in counts.values(): return HandType.FULL_HOUSE
        if 4 in counts.values(): return HandType.FOUR_OF_A_KIND
      case 1: return HandType.FIVE_OF_A_KIND

  def to_card(self, card: str) -> Card: 
    match card:
      case '2': return Card.TWO
      case '3': return Card.THREE
      case '4': return Card.FOUR
      case '5': return Card.FIVE
      case '6': return Card.SIX
      case '7': return Card.SEVEN
      case '8': return Card.EIGHT
      case '9': return Card.NINE
      case 'T': return Card.TEN
      case 'J': return Card.JACK
      case 'Q': return Card.QUEEN
      case 'K': return Card.KING
      case 'A': return Card.ACE
  
  def get_cards(self) -> list[Card]:
    return [self.to_card(card) for card in self.hand]
  
  def __lt__(self, other: Self):
    if self.__class__ is other.__class__:
      if self.type != other.type:
        return self.type < other.type
      else:
        return self.cards < other.cards
    return NotImplemented
  
  def __eq__(self, other: Self):
    if self.__class__ is other.__class__:
      return self.type == other.type and self.cards == other.cards
    return NotImplemented


def main(text: str) -> int:
  hands = sorted([Hand(*line.split()) for line in text.splitlines()])
  return sum(hand.bid * rank for rank, hand in enumerate(hands, start=1))

if __name__ == '__main__':
  print(main(Path('sample.txt').read_text()))
  print(main(Path('z_input.txt').read_text()))
