# _ゼロから作るDeep Learning_ in common lisp

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

## 進捗&日記

### ch.4

#### 2025/1/1

- Common Lispの基本構文を勉強した

- `mnist`データセットのダウンロードを行う`download-mnist.lisp`を実装。

- `mnist`データセットが、`http://yann.lecun.com/exdb/mnist/`からダウンロードできなくなっていたので、tensorflowのリポジトリから見つけたミラー(`https://storage.googleapis.com/cvdf-datasets/mnist/`)からダウンロードした。

#### 1/2

- `mnist`データセットの読み込みを行う`load-mnist.lisp`を実装。

#### 1/3

- マクロを勉強した。複数の`simple-array`をzipしてイテレートする`do-sarray`マクロを実装。

- 偏微分を実装し、勾配降下を行なう`gradient-descent`関数を実装。(`diff.lisp`内)

- 型宣言も調べたが、まだよくわっていない。

#### 1/5

- `map-into`を使って、`do-sarray`マクロを使わずに`gradient-descent`を実装しなおした。
