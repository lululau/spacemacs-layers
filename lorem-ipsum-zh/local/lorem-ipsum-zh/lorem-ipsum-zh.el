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
     "一往情深深几许？深山夕照深秋雨。")
("孤鸿海上来，池潢不敢顾。"
"侧见双翠鸟，巢在三珠树。"
"矫矫珍木巅，得无金丸惧。"
"美服患人指，高明逼神恶。"
"今我游冥冥，弋者何所慕。")

("暮从碧山下，山月随人归，"
"却顾所来径，苍苍横翠微。"
"相携及田家，童稚开荆扉。"
"绿竹入幽径，青萝拂行衣。"
"欢言得所憩，美酒聊共挥。"
"长歌吟松风，曲尽河星稀。"
"我醉君复乐，陶然共忘机。")

("燕草如碧丝，秦桑低绿枝。"
"当君怀归日，是妾断肠时。"
"春风不相识，何事入罗帏？")

("绝代有佳人，幽居在空谷。"
"自云良家子，零落依草木。"
"关中昔丧乱，兄弟遭杀戮。"
"官高何足论，不得收骨肉。"
"世情恶衰歇，万事随转烛。"
"夫婿轻薄儿，新人美如玉。")

("空山不见人，但闻人语响。"
"返景入深林，复照青苔上。")

("独坐幽篁里，弹琴复长啸。"
"深林人不知，明月来相照。")

("山中相送罢，日暮掩柴扉。"
"春草明年绿，王孙归不归？")

("终南阴岭秀，积雪浮云端。"
"林表明霁色，城中增暮寒。")

("怀君属秋夜，散步咏凉天。"
"空山松子落，幽人应未眠。")

("故国三千里，深宫二十年。"
"一声何满子，双泪落君前。")

("岭外音书绝，经冬复立春。"
"近乡情更怯，不敢问来人。")

("打起黄莺儿，莫教枝上啼。"
"啼时惊妾梦，不得到辽西。")

("松下问童子，言师采药去。"
"只在此山中，云深不知处。")

("玉阶生白露，夜久侵罗袜。"
"却下水晶帘，玲珑望秋月。")

("绿蚁新醅酒，红泥小火炉。"
"晚来天欲雪，能饮一杯无？")

("嫁得瞿塘贾，朝朝误妾期。"
"早知潮有信，嫁与弄潮儿。")

("少小离家老大回，乡音无改鬓毛衰。"
"儿童相见不相识，笑问客从何处来？")

("隐隐飞桥隔野烟，石矶西畔问渔船。"
"桃花尽日随流水，洞在清溪何处边？")

("朝辞白帝彩云间，千里江陵一日还。"
"两岸猿声啼不住，轻舟已过万重山。")

("更深月色半人家，北斗阑干南斗斜。"
"今夜偏知春气暖，虫声新透绿窗纱。")

("银烛秋光冷画屏，轻罗小扇扑流萤。"
"天阶夜色凉如水，坐看牵牛织女星。")

("桂魄初生秋露微，轻罗已薄未更衣。"
"银筝夜久殷勤弄，心怯空房不忍归！")

("劝君莫惜金缕衣，劝君惜取少年时。"
"花开堪折直须折，莫待无花空折枝！")

("玄宗回马杨妃死，云雨难忘日月新。"
"终是圣明天子事，景阳宫井又何人？")

    ))

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
