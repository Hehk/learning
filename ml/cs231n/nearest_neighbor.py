import torch
from datasets import load_dataset
import numpy as np

cifar_dataset = load_dataset("cifar10")
train = cifar_dataset["train"]
test = cifar_dataset["test"]


def print_accuracy(y_pred, y_true, k=1):
    y_true = torch.tensor(y_true)
    accurate = (y_pred - y_true) == 0
    accurate = accurate.sum().item()
    print(f"Accuracy k={k}: {accurate / len(y_pred)}")


def to_1d_tensor(tensor):
    return tensor.reshape(-1).reshape(-1)


def image_to_tensor(image):
    np_image = np.array(image).transpose(2, 0, 1)
    tensor = torch.from_numpy(np_image)
    return to_1d_tensor(tensor)


def image_list_to_tensor(image_list):
    return torch.stack([image_to_tensor(image) for image in image_list])


class NearestNeighbor:
    def __init__(self, X, y):
        self.Xtr = image_list_to_tensor(X)
        self.ytr = torch.tensor(y)

    def __call__(self, X, dist_type="L1", k=1):
        return self.predict(X, dist_type=dist_type, k=k)

    def predict_l1(self, X, k=1):
        distance = torch.sum(torch.abs(self.Xtr - X), dim=1)
        prediction = self.ytr[torch.argsort(distance)[:k]]
        if k > 1:
            return torch.bincount(prediction).argmax()
        else:
            return prediction

    def predict_l2(self, X, k=1):
        distance = torch.sqrt(torch.sum((self.Xtr - X) ** 2, dim=1))
        prediction = self.ytr[torch.argsort(distance)[:k]]
        if k > 1:
            return torch.bincount(prediction).argmax()
        else:
            return prediction

    def predict(self, X, dist_type="L1", k=1):
        X = image_list_to_tensor(X)
        num_test = X.shape[0]
        Ypred = torch.zeros(num_test, dtype=torch.int64)

        for i in range(num_test):
            if dist_type == "L1":
                Ypred[i] = self.predict_l1(X[i], k=k)
            elif dist_type == "L2":
                Ypred[i] = self.predict_l2(X[i], k=k)
            else:
                raise ValueError("dist_type must be either L1 or L2")

        return Ypred


model = NearestNeighbor(train["img"], train["label"])
# Yte_predict = model.predict(test["img"][:100])
# print_accuracy(Yte_predict[:100], torch.tensor(test["label"][:100]))
# Yte_predict = model.predict(test["img"][:100], dist_type="L2")
# print_accuracy(Yte_predict[:100], torch.tensor(test["label"][:100]))

sample_size = 500
test_imgs = test["img"][:sample_size]
test_labels = test["label"][:sample_size]

Yte_predict = model(test_imgs, dist_type="L2", k=1)
print_accuracy(Yte_predict, test_labels)

Yte_predict = model(test_imgs, dist_type="L2", k=5)
print_accuracy(Yte_predict, test_labels, k=5)

Yte_predict = model(test_imgs, dist_type="L2", k=10)
print_accuracy(Yte_predict, test_labels, k=10)
