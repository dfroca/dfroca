import numpy as np
import pandas as pd
import torch
from sklearn.model_selection import train_test_split
from sklearn.utils.class_weight import compute_class_weight
from torch import nn
from torch.optim import AdamW
from torch.utils.data import Dataset, DataLoader
from transformers import BertTokenizer, BertModel


# Transforms the input train file and dev file to a pandas dataframe
# if the dev file is none then it will return none for the dataset
def read_and_preprocess_data(train_file, dev_file, label_code):
    col_names = ["sentiment", "aspect", "term", "position", "sentence"]
    df = pd.read_csv(train_file, sep='\t', header=None, names=col_names)

    df["sentiment_code"] = df.apply(lambda row: label_code[row.sentiment], axis=1)

    if dev_file is not None:
        df_test = pd.read_csv(dev_file, header=None, delimiter="\t", names=col_names)
        df_test["sentiment_code"] = df_test.apply(lambda row: label_code[row.sentiment], axis=1)

    else:
        df_test = None

    return df, df_test


def combine_sentence(df, df_test, tokenizer):
    df["combined_sentence"] = df.apply(lambda row: row.sentence + tokenizer.sep_token + row.aspect +
                                                   tokenizer.sep_token + row.term, axis=1)

    if df_test is not None:
        df_test["combined_sentence"] = df_test.apply(lambda row: row.sentence + tokenizer.sep_token + row.aspect +
                                                                 tokenizer.sep_token + row.term, axis=1)

    return df, df_test


def data_loader(df, tokenizer, max_len, batch_size):
    ds = ABSA_dataset(
        reviews=df.combined_sentence.to_numpy(),
        targets=df.sentiment_code.to_numpy(),
        tokenizer=tokenizer,
        max_len=max_len
    )

    return DataLoader(ds, batch_size=batch_size, num_workers=2)


class Classifier:
    """The Classifier"""

    # Initializes the Classifier object and creates the common variables
    # that will be used during the training and testing
    def __init__(self):
        self.label_code = {"negative": 0, "neutral": 1, "positive": 2}
        self.label_code_inv = {0: "negative", 1: "neutral", 2: "positive"}
        self.model_name = 'bert-base-cased'  # 'bert-base-uncased'
        self.max_len = 200
        self.batch_size = 8
        self.best_model = None
        self.device = None

    def train(self, trainfile, devfile=None):
        """
        Trains the classifier model on the training set stored in file trainfile
        WARNING: DO NOT USE THE DEV DATA AS TRAINING EXAMPLES, YOU CAN USE THEM ONLY FOR THE OPTIMIZATION
         OF MODEL HYPERPARAMETERS
        """

        # Read the data
        df, df_test = read_and_preprocess_data(trainfile, devfile, self.label_code)
        # Create the tokenizer from bert with the
        tokenizer = BertTokenizer.from_pretrained(self.model_name)
        # Combine the sentences
        df, df_test = combine_sentence(df, df_test, tokenizer)
        # Split train_file into train and validation
        df_train, df_val = train_test_split(df, test_size=0.2, random_state=0)
        # Create the dataloaders
        train_data_loader = data_loader(df_train, tokenizer, self.max_len, self.batch_size)
        val_data_loader = data_loader(df_val, tokenizer, self.max_len, self.batch_size)
        # test_data_loader = data_loader(df_test, tokenizer, self.max_len, self.batch_size)

        # Load neural network classifier model
        model = nnClassifier(3, self.model_name)
        device = torch.device("cuda:0" if torch.cuda.is_available() else "cpu")
        self.device = device
        model = model.to(device)

        # Compute class weights for using in loss function as classes are imbalanced
        train_labels = df_train.sentiment_code
        # compute the class weights
        class_weights = compute_class_weight(class_weight="balanced",
                                             classes=np.unique(train_labels),
                                             y=train_labels
                                             )
        weights = torch.tensor(class_weights, dtype=torch.float).to(device)

        # Specify params for training
        epochs = 10
        optimizer = AdamW(model.parameters(), lr=2e-5)

        loss_fn = nn.CrossEntropyLoss(weight=weights).to(device)

        # Train the model - Best model is also saved in this function
        train_model(model, epochs, loss_fn, optimizer, train_data_loader, val_data_loader, device, df_train,
                    df_val)

        # Use best model to make predictions on test data
        model = nnClassifier(3, self.model_name)
        model.load_state_dict(torch.load('best_model_state.bin'))
        self.best_model = model.to(device)

    def predict(self, datafile):
        """Predicts class labels for the input instances in file 'datafile'
        Returns the list of predicted labels
        """
        tokenizer = BertTokenizer.from_pretrained(self.model_name)

        df_test, _ = read_and_preprocess_data(datafile, None, self.label_code)
        df_test, _ = combine_sentence(df_test, None, tokenizer)
        test_data_loader = data_loader(df_test, tokenizer, self.max_len, self.batch_size)

        ## Use self.best_model to make predictions
        model = self.best_model.eval()

        predictions = []
        with torch.no_grad():
            for data in test_data_loader:
                input_ids = data["input_ids"].to(self.device)
                attention_mask = data["attention_mask"].to(self.device)

                outputs = model(input_ids=input_ids, attention_mask=attention_mask)
                _, preds = torch.max(outputs, dim=1)

                predictions.extend(preds)

        predictions = torch.stack(predictions).cpu().numpy().tolist()
        preds = list(map(lambda x: self.label_code_inv[x], predictions))

        return preds


# Class created for the  custom datasets that will be trained
class ABSA_dataset(Dataset):

    def __init__(self, reviews, targets, tokenizer, max_len):
        self.reviews = reviews
        self.targets = targets
        self.tokenizer = tokenizer
        self.max_len = max_len

    def __len__(self):
        return len(self.reviews)

    def __getitem__(self, item):
        review = str(self.reviews[item])
        target = self.targets[item]

        encoding = self.tokenizer.encode_plus(
            review,
            add_special_tokens=True,
            max_length=self.max_len,
            return_token_type_ids=False,
            padding='max_length',
            return_attention_mask=True,
            return_tensors='pt',
        )

        return {
            'review_text': review,
            'input_ids': encoding['input_ids'].flatten(),
            'attention_mask': encoding['attention_mask'].flatten(),
            'targets': torch.tensor(target, dtype=torch.long)
        }


class nnClassifier(nn.Module):

    def __init__(self, n_classes, bert_model_name):
        super(nnClassifier, self).__init__()
        self.bert = BertModel.from_pretrained(bert_model_name)
        self.drop = nn.Dropout(p=0.3)
        self.fc = nn.Linear(self.bert.config.hidden_size, n_classes)

    def forward(self, input_ids, attention_mask):
        x = self.bert(
            input_ids=input_ids,
            attention_mask=attention_mask
        )
        x = x.pooler_output

        x = self.drop(x)
        return self.fc(x)


def train_model(model, epochs, loss_fn, optimizer, train_data_loader, val_data_loader, device, df_train,
                df_val):
    best_accuracy = 0

    for epoch in range(epochs):

        print('-' * 10)
        print("Epoch ", (epoch + 1), "/", epochs)
        print('-' * 10)

        train_acc, train_loss = train_one_epoch(model, train_data_loader, loss_fn, optimizer, device,
                                                len(df_train))

        print('Train loss = ', train_loss, 'accuracy = ', train_acc)

        val_acc, val_loss = eval_one_epoch(model, val_data_loader, loss_fn, device, len(df_val))

        print('Val loss = ', val_loss, 'accuracy = ', val_acc)
        print()

        if val_acc > best_accuracy:
            torch.save(model.state_dict(), 'best_model_state.bin')
            best_accuracy = val_acc
    print("Best val accuracy achieved = ", best_accuracy)


def train_one_epoch(model, data_loader, loss_fn, optimizer, device, n_examples):
    model = model.train()

    losses = []
    correct_predictions = 0

    for data in data_loader:
        input_ids = data["input_ids"].to(device)
        attention_mask = data["attention_mask"].to(device)
        targets = data["targets"].to(device)

        outputs = model(input_ids=input_ids, attention_mask=attention_mask)

        _, preds = torch.max(outputs, dim=1)
        loss = loss_fn(outputs, targets)

        correct_predictions += torch.sum(preds == targets)
        losses.append(loss.item())

        loss.backward()
        nn.utils.clip_grad_norm_(model.parameters(), max_norm=1.0)
        optimizer.step()
        optimizer.zero_grad()

    return correct_predictions.cpu().numpy() / n_examples, np.mean(losses)


def eval_one_epoch(model, data_loader, loss_fn, device, n_examples):
    model = model.eval()

    losses = []
    correct_predictions = 0

    with torch.no_grad():
        for d in data_loader:
            input_ids = d["input_ids"].to(device)
            attention_mask = d["attention_mask"].to(device)
            targets = d["targets"].to(device)

            outputs = model(input_ids=input_ids, attention_mask=attention_mask)
            _, preds = torch.max(outputs, dim=1)

            loss = loss_fn(outputs, targets)

            correct_predictions += torch.sum(preds == targets)
            losses.append(loss.item())

    return correct_predictions.cpu().numpy() / n_examples, np.mean(losses)
