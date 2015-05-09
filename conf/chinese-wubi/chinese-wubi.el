;;; chinese-wubi.el --- Chinese input method - Wubi (五笔) -*- coding: utf-8 -*-

;; Copyright (C) 2005 Yuwen Dai
;; Copyright (C) 2005 William Xu
;; Copyright (C) 2014 Zhitao Gong

;; Authors: Yuwen Dai <daiyuwen@freeshell.org>
;;          William Xu <william.xwl@gmail.com>
;;          Zhitao Gong <me@gongzhitaao.org>
;; Keywords: i18n

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; How to install:

;; Place the chinese-wubi.el and the chinese-wubi-rules.el in your
;; load path, and add the following in your .emacs file:

;; (require 'chinese-wubi)

;;; Code:

(require 'quail)

(quail-define-package
 "chinese-wubi" "Chinese-GB" "五笔字型"
 '((121 . "言y")
   (120 . "纟x")
   (119 . "人w")
   (118 . "女v")
   (117 . "立u")
   (116 . "禾t")
   (115 . "木s")
   (114 . "白r")
   (113 . "金r")
   (112 . "之p")
   (111 . "火o")
   (110 . "已n")
   (109 . "山m")
   (108 . "田l")
   (107 . "口k")
   (106 . "日j")
   (105 . "水i")
   (104 . "目h")
   (103 . "王g")
   (102 . "土f")
   (101 . "月e")
   (100 . "大d")
   (99 . "又c")
   (98 . "子b")
   (97 . "工a"))

"汉字输入∷五笔字型∷

Created by Dai Yuwen (daiyuwen@freeshell.org).
Modified by Zhitao Gong (me@gongzhitaao.org).

	五笔字型汉字编码方案

  键  区  码  键名  笔形、基本字根

   g     (11)  王   一,五,戋
   f  横 (12)  土   二,士,十,干,寸,雨
   d  起 (13)  大   三,犬,石,古,厂
   s  类 (14)  木   西,丁
   a     (15)  工   匚,七弋,戈,廾艹廿

   h     (21)  目   上,止,卜
   j  竖 (22)  日   曰,早,虫,刂
   k  起 (23)  口   川
   l  类 (24)  田   甲,囗,四皿,车,力
   m     (25)  山   由,冂,贝,几

   t     (31)  禾   丿,竹,彳,攵夂
   r  撇 (32)  白   手扌,斤
   e  起 (33)  月   彡,月,用,乃,豕
   w  类 (34)  人   亻,八
   q     (35)  金   金钅,勹夕犭,儿

   y     (41)  言   讠,亠,广,文,方
   u  捺 (42)  立   冫丬,六,辛,疒,门
   i  起 (43)  水   氵,小
   o  类 (44)  火   灬,米
   p     (45)  之   辶廴,冖宀

   n     (51)  已   乙,已己巳,尸,心忄,羽
   b  折 (52)  子   孑,凵,了,阝,耳,卩,也
   v  起 (53)  女   巛,刀,九,彐,臼
   c  类 (54)  又   厶,巴,马
   x     (55)  纟   幺,弓,匕

    键名谱
  (横区): 工 木 大 土 王
  (坚区): 目 日 口 田 山
  (撇区): 金 人 月 白 禾
  (捺区): 言 立 水 火 之
  (折区): 已 子 女 又 纟

    助记词
  G 王旁青头戋五一
  F 土士二干十寸雨
  D 大犬三羊古石厂
  S 木丁西
  A 工戈草头右框七

  H 目具上止卜虎皮 (具上 指具字的上部 且)
  J 日早两竖与虫依
  K 口与川, 字根稀
  L 田甲方框四车力
  M 山由贝, 下框几

  T 禾竹一撇双人立, 反文条头共三一
  R 白手看头三二斤 (三二 指键为 32)
  E 月彡乃用家衣底
  W 人和八, 三四里 (三四 即 34)
  Q 金勺缺点无尾鱼, 犬旁留X儿一点夕, 氏无七(妻)

  Y 言文方广在四一, 高头一捺谁人去
  U 立辛两点六门病
  I 水旁兴头小倒立
  O 火业头, 四点米
  P 之宝盖, 摘(示)(衣)

  N 已半巳满不出己 左框折尸心和羽
  B 子耳了也框向上
  V 女刀九臼山朝西
  C 又巴马 丢矢矣
  X 慈母无心弓和匕 幼无力
"
'(("\C-?" . quail-delete-last-char)
  (" " . quail-select-current))
nil nil nil nil)

(load "chinese-wubi-rules.el")

(provide 'chinese-wubi)
;;; chinese-wubi.el ends here
