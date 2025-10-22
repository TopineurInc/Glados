"""
Exercice 18 - Fusion de listes
"""

__author__ = "login_x"


def merge_lists(list1: list, list2: list) -> list:
    """Fusionne deux listes sans doublon et les trie par ordre croissant."""
    return sorted(set(list1 + list2))
