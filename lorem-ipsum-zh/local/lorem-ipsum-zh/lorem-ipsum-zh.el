;;; lorem-ipsum-zh.el --- Insert dummy pseudo Latin text.
(defconst lorem-ipsum-zh-version "0.2")

(defgroup lorem-ipsum-zh nil
  "Insert filler text."
  :group 'tools
  :group 'convenience)

(defconst lorem-ipsum-zh-text
  '(("千古江山，英雄无觅，孙仲谋处。"
     "舞榭歌台，风流总被，雨打风吹去。"
     "斜阳草树，寻常巷陌，人道寄奴曾住。想当年、金戈铁马，气吞万里如虎。"
     "元嘉草草，封狼居胥，赢得仓皇北顾。"
     "四十三年，望中犹记，烽火扬州路。"
     "可堪回首，佛狸祠下，一片神鸦社鼓！凭谁问、廉颇老矣，尚能饭否？")
    ("豫章故郡，洪都新府。"
     " 星分翼轸，地接衡庐。"
     "襟三江而带五湖，控蛮荆而引瓯越。"
     "物华天宝，龙光射牛斗之墟；人杰地灵，徐孺下陈蕃之榻。"
     "雄州雾列，俊彩星驰。"
     "台隍枕夷夏之交，宾主尽东南之美。"
     "都督阎公之雅望，棨戟遥临；宇文新州之懿范，襜帷暂驻。"
     "十旬休暇，胜友如云。"
     "千里逢迎，高朋满座。"
     "腾蛟起凤，孟学士之词宗；紫电青霜，王将军之武库。家君作宰，路出名区。童子何知？躬逢胜饯。")
    ("今古河山无定据，画角声中，牧马频来去。。"
     "满目荒凉谁可语？西风吹老丹枫树。"
     "从来幽怨应无数？铁马金戈，青冢黄昏路。。"
     "一往情深深几许？深山夕照深秋雨。")))

(defvar lorem-ipsum-zh-paragraph-separator "\n\n")
(defvar lorem-ipsum-zh-sentence-separator "")
(defvar lorem-ipsum-zh-list-beginning "")
(defvar lorem-ipsum-zh-list-bullet "* ")
(defvar lorem-ipsum-zh-list-item-end "\n")
(defvar lorem-ipsum-zh-list-end "")

(make-variable-buffer-local 'lorem-ipsum-zh-paragraph-separator)
(make-variable-buffer-local 'lorem-ipsum-zh-sentence-separator)
(make-variable-buffer-local 'lorem-ipsum-zh-list-beginning)
(make-variable-buffer-local 'lorem-ipsum-zh-list-bullet)
(make-variable-buffer-local 'lorem-ipsum-zh-list-item-end)
(make-variable-buffer-local 'lorem-ipsum-zh-list-end)

(add-hook 'sgml-mode-hook (lambda ()
			    (setq lorem-ipsum-zh-paragraph-separator "<br><br>\n"
				  lorem-ipsum-zh-sentence-separator "&nbsp;&nbsp;"
				  lorem-ipsum-zh-list-beginning "<ul>\n"
				  lorem-ipsum-zh-list-bullet "<li>"
				  lorem-ipsum-zh-list-item-end "</li>\n"
				  lorem-ipsum-zh-list-end "</ul>\n")))

;;;###autoload
(defun lorem-ipsum-zh-insert-paragraphs (&optional num)
  "Insert lorem ipsum paragraphs into buffer.
If NUM is non-nil, insert NUM paragraphs."
  (interactive "p")
  (if (not num)(setq num 1))
  (if (> num 0)
      (progn
	(insert (concat
		 (mapconcat 'identity
			    (nth (random (length lorem-ipsum-zh-text))
				 lorem-ipsum-zh-text) lorem-ipsum-zh-sentence-separator) lorem-ipsum-zh-paragraph-separator))
	(lorem-ipsum-zh-insert-paragraphs (- num 1)))))

;;;###autoload
(defalias 'Lorem-ipsum-zh-insert-paragraphs 'lorem-ipsum-zh-insert-paragraphs)

;;;###autoload
(defun lorem-ipsum-zh-insert-sentences (&optional num)
  "Insert lorem ipsum sentences into buffer.
If NUM is non-nil, insert NUM sentences."
  (interactive "p")
  (if (not num)(setq num 1))
  (if (> num 0)
      (progn
	(let ((para
	       (nth (random (length lorem-ipsum-zh-text)) lorem-ipsum-zh-text)))
	  (insert (concat (nth (random (length para)) para) lorem-ipsum-zh-sentence-separator)))
	(lorem-ipsum-zh-insert-sentences (- num 1)))))

;;;###autoload
(defalias 'Lorem-ipsum-zh-insert-sentences 'lorem-ipsum-zh-insert-sentences)

;;;###autoload
(defun lorem-ipsum-zh-insert-list (&optional num)
  "Insert lorem ipsum list items into buffer.
If NUM is non-nil, insert NUM list items."
  (interactive "p")
  (if (not num)(setq num 1))
  (if (> num 0)
      (progn
	(let ((para (nth (random (length lorem-ipsum-zh-text)) lorem-ipsum-zh-text)))
	  (insert (concat lorem-ipsum-zh-list-bullet
			  (nth (random (length para)) para)
			  lorem-ipsum-zh-list-item-end)))
	(lorem-ipsum-zh-insert-list (- num 1)))
    (insert lorem-ipsum-zh-list-end)))

;;;###autoload
(defalias 'Lorem-ipsum-zh-insert-list 'lorem-ipsum-zh-insert-list)

(provide 'lorem-ipsum-zh)

;;; lorem-ipsum-zh.el ends here
