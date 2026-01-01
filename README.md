# ゼロから作るDeep Learning in common lisp

## 概要

LispとDeep Learningの勉強のため、O'Reilly Japanの「ゼロから作るDeep Learning」をCommon
Lispで実装しようとしています。

## 環境

### Lisp処理系とか

- SBCL
- Quicklisp

### ライブラリ

- OpenBLAS (行列計算用)
- CFFI (OpenBLASの呼び出し用)

## 進捗&メモ

### ch.4

- `mnist`データセットのダウンロードを行う`download-mnist.lisp`を実装。

- `mnist`データセットが、`http://yann.lecun.com/exdb/mnist/`からダウンロードできなくなっていたので、tensorflowのリポジトリから見つけたミラー(`https://storage.googleapis.com/cvdf-datasets/mnist/`)からダウンロードした。
