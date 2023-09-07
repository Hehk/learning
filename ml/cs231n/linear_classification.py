import torch
from datasets import DatasetDict, load_dataset
import numpy as np

cifar_dataset: DatasetDict = load_dataset("cifar10")
train = cifar_dataset["train"]
test = cifar_dataset["test"]


def svm_loss(x, y, W, Δ):
    scores = W.dot(x)
    return (scores - scores[y] + Δ).clamp(0).sum() - Δ


def weight_regularization(W, λ):
    return (W * W).sum() * λ




