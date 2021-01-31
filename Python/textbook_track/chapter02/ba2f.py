# Implement RandomizedMotifSearch
import random
import sys

from collections import Counter, namedtuple

NUCLEOTIDES = ('A', 'C', 'G', 'T')


class ProfileColumn(namedtuple('ProfileColumn', NUCLEOTIDES)):
    def argmax(self):
        _, nucleotdie = max(zip(self, self._fields))
        return nucleotdie


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def calc_hamming_distance(s1, s2):
    distance = 0
    for c1, c2 in zip(s1, s2):
        if c1 != c2:
            distance += 1
    return distance


def create_profile_matrix_from_motifs_using_pseudocounts(motifs):
    nr_motifs = len(motifs)
    profile_matrix = []
    for column in zip(*motifs):
        counts = Counter(column)
        values = tuple((counts.get(nucleotide, 0) + 1) / (nr_motifs + 4) for nucleotide in NUCLEOTIDES)
        profile_matrix.append(ProfileColumn(*values))
    return profile_matrix


def calc_profile_matrix_score(motifs):
    profile_matrix = create_profile_matrix_from_motifs_using_pseudocounts(motifs)
    consensus = ''.join(column.argmax() for column in profile_matrix)
    score = 0
    for motif in motifs:
        score += calc_hamming_distance(motif, consensus)
    return score


def calc_k_mer_probabilty(k_mer, profile_matrix):
    p = 1.0
    for nucleotide, column in zip(k_mer, profile_matrix):
        p *= getattr(column, nucleotide)
    return p


def profile_most_probable_k_mer(text, profile_matrix, k):
    max_probability = 0.0
    most_probable_k_mer = text[:k]
    for ix in range(len(text)-k+1):
        k_mer = text[ix:ix+k]
        p = calc_k_mer_probabilty(k_mer, profile_matrix)
        if p > max_probability:
            max_probability = p
            most_probable_k_mer = k_mer
    return most_probable_k_mer


def select_random_motifs(texts, k):
    motifs = []
    for text in texts:
        ix = random.randint(0, len(text)-k)
        motif = text[ix:ix+k]
        motifs.append(motif)
    return motifs


def randomized_motif_search(texts, k):
    motifs = select_random_motifs(texts, k)
    best_motifs = None
    best_score = k * len(texts)
    while (score := calc_profile_matrix_score(motifs)) < best_score:
        best_score = score
        best_motifs = motifs
        profile = create_profile_matrix_from_motifs_using_pseudocounts(motifs)
        motifs = [profile_most_probable_k_mer(text, profile, k) for text in texts]
    return best_motifs, best_score


def run_randomized_motif_search(texts, k, nr_iterations):
    random.seed(2112)
    best_motifs = None
    best_score = len(texts) * k
    for _ in range(nr_iterations):
        motifs, score = randomized_motif_search(texts, k)
        if score < best_score:
            best_score = score
            best_motifs = motifs
    return best_motifs


def main():
    data = sys.stdin.read().splitlines()
    k, t = convert_to_intlist(data[0])
    texts = data[1:t+1]
    result = run_randomized_motif_search(texts, k, nr_iterations=1000)
    for motif in result:
        print(motif)


if __name__ == '__main__':
    main()
