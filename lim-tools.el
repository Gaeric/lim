;;; -*- coding: utf-8 -*-
;;; lim-tools -- lim-tools for chinese

;; 1. evil find enhance for chinese by lim.
;; 2. count chinese words

;; Compatibility: Emacs 26.1
;; Copyright 2018
;; Author: lantian
;; version 0.07
;; Description: Ligthly Input Architecture

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; 1. evil find enhance for chinese by lim.
;; 中文说明
;; 对evil的find功能(f/F/t/T)功能进行增强，使其可以通过码表的首字母跳转至中文字符处
;; 首先，建立字符与汉字对应关系表 lim--char-cn-lib

;; 词条的结构应该是：

(defvar lim--char-cn-lib
  '(("y" . "原尤右殒餍硬厌靥砑雁魇殃赝厣奄砚研压殪愿有郁欹已熨异尉鬻尹尾引聿肀样榆椅桠椰杳柚樱樾乙橼杨檐楹游淤演液瀛洋浴淫溢瀹漾渝源一淹涯湮滢渊沂潆油泱洇沅滟涌泳渔沿漪页菸薏营荧蓥芽荑莠萸莜英萤茵莹茔芋芸芫艺苡蔚蓣蕴萦荫胤药莺莸苑茚要雨燕亚焉酉酽严颐鞅酝耶鄢鄞酏元玉瑶琰鼋瑗瑜珧琊瑛运迂盂韫璎耘于玥云艳言衣亦瘀霪应癔痒饔兖奕痍疣疟痖弈瘗夜颜庾瘐旖彦赢裔音瘿壅雩育冶膺雍庸翊瘾毅韵鹰嬴疡痈疫意叶咬唁喑唷噫吟喻呀咦哑呓跃咿员喁咽嘤哕吁噎咏吆踊郢吲郧邑哟呦依佯仪阅俣佾优佑伛阈倚俨伢偃阉侑攸佚仡逾龠觎余闫伊愈悠耀俑俞亿仰阎佣摇衍掖援揖徭徉揄挹掩揠衙揶御押拽撄掾扬拥抑役语永翌谳谚谊谣议以裕谕讶祐羿预矣谀诱翼甬谒祎译恿诒羽予郓勇冤鹬豫诣允爷义炎爻焱谷烊肴猷养烨舀燠烟遥煜益釉羊丫刈剡爰熠恙欲鹞鹆焰炀繇远誉窨慵愉悦悒窬忧寅恹宥窑窳宜寓怏愠宴宇忆怡恽怿窈眼央禺蝣圆蚁峪崤蝇蛘蝓嵛崖因岩圄蜮屿由岈螈蚜罨崦睚囿崾蜒曳罂屹遇遗圜蚰蜴园圉野愚贻眙蛹邮峄蚓幽黝蚴婴鸭鸯鹦嶷黟友舣殷易禹臾昱晏舆曰晕舁粤晔影映邀鼹鼬岳劓延牖曜昀与夷弋娱彧媛姚姨妪娅嫣尧雅医妖轶牙姻迓辕妍轧郾翳妤鸦欤轺鸢移夭毓箢氧氩筵秧迤氤氲筠竽越懿垸墉袁业域壹垭塬垣堰堙埏盐埸垠邺圯月院腋臆臃陨媵腰胰腌腴阳隅胭刖阽阴也隐用匀狺夤鳙馀鳐狳犹鱿猗颖颍饫迎逸鱼眢猿印怨饴狱肄疑狁饮鸳约幺验彝缢邕缨纭纡缘驭绎驿幼银镛铱镱虞镒铫铘钺龉铕铀铟卣钰钥龈钇又盈孕")
    ("x" . "雄袭碹燹戌硖咸硒厢厦硝硎夏矽殉弦寻巽屣遐屑犀相校楦杏枵械朽析皙榭檄迅柙楔樨想榍栩橡消漩渲泫溪淆浠汹溆泄渫淅湘洗溴洫潇涎瀣泻浔汛洵汐萧薪莘萱蓿蓄莶蕈菥葙荤薤薛薰荇蓰苋荥葸芯芗芎荀藓西熙下醯酗献项醺髹靴酰醒昔觋鞋巷孝醑协型璇瑄砉顼形刑瑕邢新辛痃享襄庠亵霰需效庥旋冼席畜霄痫序玄燮雪霞歆癣廨响哮喧躞唏蹊吓哓嗅嚣咻跹跣呷嘘嘻噱吁啸兄吸勋像斜信鸺闲休侠偕阋仙逍儇削僖叙肖翕修鹇脩行揎徐踅挟循擤衔掀携徙撷徇许熊心祥详谖戏习谐谢训袖裼谑祆禧禊讯褶诩询羡煊炫烯希粞兮羲貅熄籼羞悉奚凶郄翔糈欷鲞性窨穴兴窸宣惜恤宿忻宪惺泶宵悻恂穸懈现瞎眩蟋骱峡虾蜥螅蝎岫蚬岘黠盱蛸县悬峋勖蟓小省欣舷舾须息暄煦衅囟向晞曦鼷血臭晓晰星曛昕暹显魈暇歇舄写辖嫌姓媳匣轩嬉娴邪絮婿先熏稀箱牺筱笑筅选香氙罅箫卸秀喜熹幸埙馨贤墟学险膝腺逊腥隰隙限胥陉胸胁降陷系夕鲜猃匈馐狭枭鳕饷獯饩象蟹邂旭旬猩狎玺鲟郇馅獬线续骧绪飨骁缃绁乡纤驯绣细巡绡缬绚些镶锌镟铉鑫锈铦铣锨锡虚销勰")
    ("w" . "碗戊威歪硪兀尢屋尉尾慰枉梧杌桅温湾汶豌涡沩鋈洧浯沃渭涠汪洼污渥潍莞蕹菀蓊莴葳薇萎苇芜蔚芴芄五吾斡巫軎卧未琬玟王顽违武韦玮玩鹉文雯痦痿瘟庑望亡忘紊弯雾妄嗡唔唯喂味吴哇喔吻呜位伪问阌闻佤伍侮仵倭偎伟闱及往挖捂挝微握挽误诬诿谓袜骛鹜婺为烷瓮焐煨炜翁完惋窝悟惟忤惘剜怃寤宛蜿蚊畹网罔蜗蜈崴畏囗巍帷嵬围蛙帏胃幄乐艉晚皖晤鼯魍旺璺韪万婉瓦娲辋娃妩娓娩舞我牾魏逶迕午委物稳无圩圬坞卫腕阢隗隈腽肟务丸乌鲔勿猥猬刎危邬维纹绾毋纬纨外龌钨解")
    ("v" . "$")
    ("t" . "态砣太餮砼殄套厅碳退屠弹屉臀桃檀柁梯梃柝桐榻樘桶椭潼溏沱涂烫滔涕洮潭汰汀沲沓湍漯溻添淌涛滩汤澹淘荡葶荼荑莛茼萜薹藤苔萄苕菟酡醣酴橐酞覃酮醍忑髫瑭天瑱吞韬替泰忝耥痛痰亭庭霆童唐庹瘫疼知啼蹄嚏跎跳饕鼍呔唾嗒踏蹋踢吐蹚叹跆嗵啕趿叨体停阗僮佗偷佻棠仝闼途贪氽侗堂倘傥佟倜提搪挑拓挞托推挺拖徒抟掭抬探摊捅投掏调台炱谈祧讨谭袒通甬裼褪邰逃糖誊羰剃鼗烃煺郯妥鹈头窕悌突恬惕恸惝它瞳疃螗眺蜕田炭团町囤蜓畋峒同贴帖螳骰蜩他听艇香暾题遢躺剔昙她婷帑忒慝屯笑毯甜箨颓透筒舔廷乇秃笤特填塘坨土塔坦塌坛趟坍堍脱陀腾粜滕豚肽腆膛腿胎陶条鸵饨獭彤兔逖鳎鲐饧鲦统驼绨驮彖缇绦铁铊钭铴锑锬凸钛镡铽铤钿铜钽镗钍忐龆图迢")
    ("s" . "死碎戍豕奢石爽厮厍厦殊硕殇盛蜃砷砂卅磉耍肃司屎尸属尿刷巳梳术柿桫松栓森枢椹虱榫杉述梢树梭栅杓深裟水渖溯泷沭涑淞洒澌潸滠漱挲潲溲湿泗濉汕沙澍涉淑沈渗涮汜鲨娑汤涩蓑莘蓍莎薮蒴荽菘葚蔌薯莳蒜苫菽萨蔬荪苏芍芟世束甚粟酾斯散酥速贳啬事十索酸肆丧三寿珅耜示琐素瑟珊市熟庶痧霎商霜竦凇施麝瘦衰疝塾瘙瘆飒孰韶嗦嗾嗜噻唼啥嗍嗽哂嘶嗄噬嗖虽呻哨唢嗉嗣唆嗓咝唰吮跚使什隼闪俗尚售僧伞耸僳闩伤傻毹伸赏畲舍侍仕仨佘似食怂伺俟舒倏歙手摔誓擅授拾擞损拴搠撕拭揲撒摄搜折逝捎摅扫搡掺搔抒势说谇识谥禅讼谂试桑叁谁诉颡衫毵诜视谡神讪诗圣社双诵扇祀设数熵兽善燧杀槊炻烧弑首颂送遂刹塑剡受羧朔糁释鄯煽烁舜实慎窣忪守宋悚室慑宿赛邃审塞恃愫山史赎嵩署蟀蛇瞬蟮赊髓崧睡睢嵊瞍螋申墅蛳眭蛸睃思岁赡蜀兕四师少省帅所艏艘舢艄身暑射是晟时晒顺晌曙鼠叟式婶嬗输轼孀娠嫂姝始恕姒姗妁失矢穑舌税黍穗秫筮生簌牲笙适眚甥升筛剩稍筲乘私笥笋矧舐歃声竖墒士寺赦垧埏埘螫肾埽随疏膻臊膳脎隧腧隋陕收胜腮胂孙祟色煞勺狩鳝猞飧夙觫馓飕稣饰馊觞蚀鲥鳃删狮氏殳饲狻鲺鳋狲甩书缩绳缮缲绶绥丝绅驶驷缌纱绱骚缫纾骟绍鸶上铈铩锼钐锶锁叔铄铯算召邵劭")
    ("r" . "辱砹碍厄鹌鸸弱屙榕桉桡枘融溶濡染沤洱溽澳润汝洳若蓉艾萼荣葚茸蓐薷荛蕤苊荏莪芮苒蒽藕惹蔼蕊茹早鞍噩耳髯垩恶鞣二熬鏊瑷珥獒聱敖瑞耦螯遨骜韧鳌这哀鏖癌庵瓤颥廒霭啊嚷嗳嗌吖呃呕嚅嗄哦颚哎噢喏嗯嗷喔蹂唉鄂鹗人阏儒佴俺俄偌偶仁闰傲恁仍仞热按攘揞扰扼摁捱挨揉拗扔认谙禳谔柔讴褥襦袄衽讹诶冗欸爱熔入乳糅燃安愕案怄额懊容黯禺肉凹蚵冉蠕岸峨蛾嵘蝾盎蚋蚺恩而日暗暧奥遏皑翱昂软嫒戎瓯娆轭娥妊媪贰婀欧轫鸥殴任氨穰稔壬矮箬岙鹅如壤埯埃坳阿肮胺隘腭孺肜阮朊儿然鳄饶饵狨鲕饿饪迩犴尔绕绒缛纫让镕锿铵锐锷铒锇睿锕铷忍刃")
    ("q" . "确犬磲奇硗砌牵戚碛丌群强屈权樯枪檎棋栖槭桥樵楸橇桤杞榷清泣洽漆渠柒淇沏汽湫汔泅潜浅沁擎蔷蕖荠荃葺蕲芩茜檠萁萋芊颧荞芹苘蘧葜茕荨芑茄芡芪取裘求鬈銎跫巧乾鞒醛逑蛩耆鞘綦期邛娶欺麴鹊勤青琼琴球契琪琦秦挈亓璩玘齐痊庆亲麒弃凄旗癯敲妾瘸跂呛噙跄器跷嘁嗪吣全禽衾佥阙俅戗倾侨仟阒企劁倩俏侵阕仇抢擒揿撬扦衢掮愆掐请巯启谦诠祺祈谯讫谴逡诮祛袪裙襁祁綮诎劝其炝酋拳糗遒羌炔羟前歉券情寝褰恰慊惓窍惬窃憔愀搴悄悭怯穹悛骞穷且岐黥蛴髂圈囚蝤蜣蜷黔曲畎赇岖蜞崎嵌瞧瞿蚯氍蛐遣蛆蠼峭螓畦岍蜻圊岂黢虬屺雀颀泉躯丘晴邱鼽七嫱区辁椠顷堑翘轻妻秋签筌箧箝筇稽乔迁迄箐千氰缺憩氢乞气去墙謦趣磬罄圻趄悫起趋壳却腔脐亟戕爿阡朐肷卿鳍欠鳅鲭犰鸲劬骑缉缲绻驱骐绮缱钱歧虔锵铨钤钳锹龋钎觑锲锖镪锓钦铅切")
    ("p" . "碰磻殍砰丕砒邳硼破匏强譬辟屏屁甓劈朴榜榀攀枰枇棚杷派滂潘湓泮漂浦溥沛湃濮瀑淠澎婆泼泡蓬萍菩蒲蒎莩苹莆苤芘葩葡配醅瓢聘票剽飘琶耪琵珀璞耙珮旁烹霈庞痞旆裒剖平霹癖冯疲庖庀疱跑喷品趴啪呸嘌啤蹒哌噗蹼嘭噼蹁咆凭仳俜仆堡俳偏僻佩排彷掊拼扒撇批抔抨拍扳扑捧徘擗抛披评谱袢襻裨扁翩谝袍普瓶粕貔叛盆贫番暼瞥判鄱炮怕怦盼赔螃畔蟠毗圃瞟螵蚍帕蜱罴蟛帔盘磐槃爬片乓皤牌乒魄迫曝郫匹嫔姘叵嫖媲娉篇氆笸筢氕犏篷牝坡培坪埔坯鼙埤彭圮堋少陪膀疋胖胼脬脯胚脾陴爿膨朋鹏陂匍鲆逄刨狍骗骈辔缥纰频裴锫镨铺钷颦钋镤钯攴铍皮颇")
    ("n" . "家耐硇奈碾恧孬尿尼想楠柠柰水泞涅淖浓溺泥蔫蘖萘孽茑艿南聍囊廿颞聂酿于弄耨辇瑙应疟霓旎凝哪喃咛蹑囔嗫喏呐啮哝呢呶念闹倪佞您侬恁傩伲拿拧捻挠攮捺捏撵拈拟扭挪搦拗难农诺讷衲那糯逆粘宁恼懦甯忸怩内蝻蛲睨男囡嬲须最曩暖臬昵衄女嫩匿胬娘怒奴妞辗孥驽娜弩妮奶努妳年牛黏氖举垴埝赧坭能脑腩腻陧肭脓脲你狞袅馁鲶鸟馕猊鲵鲇猱狃经纳纽虐镎铙镊镍锘钠钮铌钕乃鼐")
    ("m" . "面硭礞码殁民弭愍弥模木买梅枚棉檬杪楣懋杩梦灭渑沐湎沔满漭漠濛漫汨淼渺沫泌懑溟湄泯泖浼芒茫蘑蘼藐茂甍莽摹颟莓莫苗蔑暮瞢苜蔓幕萌蒙慕茉墓么荬蓦茅苠募茗鹋茆有卖醚某髦耄酶酩鞔精末玟耱玫瑁玛珉麦魔糜麋霾麻磨摩旄瘼霉庙蛮盲亩靡邙縻氓袤吗嘛嘧咪咩喵黾唛骂哞鸣门闵命仫闽闷侔描摸抹扪抿拇公谜鍪谋谟牟谩冥蝥幂瞀蟊矛袂谬谧祢美米煤貊貌敉貘迷邈熳觅焖貉忙寞懵密蜜慢悯寐宓目虻眯默皿眄瞒蟒螨瞄蟆蠛蠓冒帽幔墨眇睦瞑蚂眸蛑嵋螟蜢眠岷冕峁艨艋明曼盟魅昧暝昴妈姥嬷媒嫫迈嫚妹妙媚姆娩劢每敏牧穆牦篾秒秣牡毛秘毪鳘们埋墁坶前脒陌腼朦膜眉孟脉勐鹛名猫馍免贸馒鳗猛犸猸卯猕勉马母缅绵缈缦糸缪缗乜没镁锚镆钼镘芈钔锰镅铭铆分")
    ("l" . "怎龙烈裂砬硫殓磷厉奁砻磊聋砺厘列垄碌历砾尥鹩励灵隶屡履逯录麓椋檩栏柃楼林栊楝檑栳枥楞椤棱榈榄栌李棂榔〇柳栎婪榴橹流漉濂滦漓浏沦粱鎏潋鬲泠潞梁涞泷淋溧涟潦洌沥濑漤涝丽逦泪澧滤澜滥泸浪漏郦渌泐洛泺鹂溜落蓠蒗苓蔹蒌莲莱茏蕾苈莉莅藜荦萝蓝菱蔺芦莨蓼劳荔两吏联酹聆栗醴剌鬣醪赖酪勒理璃耒琉璐玲耧琳珑琏嫠耢靓琅珞立凉来凛廪冷六廉吝銮零露瘘麟鹿辣栾霖疠冽瘌雳癞疬旅旒挛痢痨雷离赉脔峦瘰刘凌斓率恋蠃廖孪膂庐疗羸酃廊癃亮鸾娈瘤路吕呤喽啦咙啉嘹喱咧喇呖嘞唠躏哩啰踉唳躐啷另跞咯鹭叻噜令佬侣闾伶伦偻例俩瓴阑傈僚俪领敛俐赁倮俚儡邻阆翎仑仂拉掠搂捋拦拎抡撩拢徕擂捞撂摞揽掳律捩撸论良谅褛裣戮戾裢裸垒诔谰褴礼朗郎禄鹨类料粒烂羚炼粝燎遴兰粮炉粼烙娄熘懔怜懒寮牢愣悝帘寥窿里蠊詈罹罱髅岭囹蛉蝼嵝嶙瞵囵睐崃瞭蛎蜡崂蜊逻螺累螂罗略岚赂老劣诉舻量晾魉旯军姥辘轮辚辆娌连嫘轳辂轹利笠簏篱黎稆篓笼梨篥籁犁箩黧篮乱稂箓氯氇社垃墚埒趔垅坜临览塄垆了臁脸脶膦胧陇腊陋辽陆陵胪肋隆聊乐鳞猎獠鲡鲢鳓雒猁留遛鲁狸鲤鳢猡鲈鲮狼卵馏饹络缡缕纶练缭骊骡邋蠡缧绫缆绿驴骆绺骝虑镰锍铃铝锊龄镂链铼镣镭铑颅铹锣卤锂镙镧卢锒钌镏鸬虏镥力")
    ("k" . "矿夼盔奎磕刳夸砍据垦恳尻框枯柯栲楷棵槛渴溃溘苦蔻苛蒈蒉蒯莰葵芤的可考戡髡酷聩醌恐克勘开珂琨刊亏刻廓库疴颏堃康邝亢口喀咳吭哙跨哭哐喹喟啃咔嗑跬叩咖推伉倥阔闶侃侩龛侉傀阃佧阚郐佝控抗扩扣扛抠拷揩挎括捆揆课裤诓裉诳义烤糠炕夔快慷宽喾愧忾悝窠悃愦恺寇窥慨窟恪况骷崆髋贶困岢眍眶蝰嵌颗蝌蛞髁瞌剀岿睽瞰岂凯抓得旷魁昆暌找轲匮匡看科犒箜筷氪筐筘篑稞靠待坑圹堪坷垮坤逵垲块堀款坎壳报脍胯胩孔狂馗狯馈鲲空纩绔缂骒肯卡铐钪锴钶铠锞锟铿锎龈客")
    ("j" . "厥硷戛碱礓歼礁碣劂剞丌殛厩矶尽居局君既疆屐屦犟届暨剧建忌弪郡己机杰榉检楫楷橛槿柩栉桕枧禁桔槛椒橘椐楗枷极枸桷渐济涓湔酒渠浃江豇浇激湫洎溅沮洁浅涧津浚泾浸汲洚节警茭茳蒺荠菅芰蒋蒟芥蒹莒蕺荐荚蕨苣敬蕉茧苴菌荆藉菁茎荩葭菊芨茄蓟艽做兢柬酵戟棘戬救教聚醮截鬏鞯靳晋贾赍堇觐基髻墼醵鄄嘏鞠鞫进戋瑾靓击珏耩井琚玦恝珺静玖珈玑决夹霁瘠京交疥竞减奖就桨酱廑疾颊麇旌迹浆疽竭靖齑剂脊竣痉郊疖郏瘕竟疚痂将鹫净麂叫跻跤哜嚼嗟距戢噘蹶喈噤踽噍啾旧咀践咭噱趼唧踞踺跽叽跏鹃今焦佼僦伎价俭倦集侥偕借僭僵介僬儆牮俱偈剑佶佳假仅倨俊健倔伽鹪隽阄接技挤搅捡搛捐捷挟拣撅拒掎徼揪挢揭攫街拮径抉据捃掘拘掬记计矜袷衿谫诫谏讵谨军襟颈迳扃讲诘裥讦刭肩艰诀裾谲鸡劲讥皲煎爝兼粳桊炬羯炯眷蠲精卷烬糨爵翦焗姜鹣剪觉惊謇蹇窭寄家悸窖惧憬寂举窘究见蛟嵴界具睑蚧囧圈畸睫蛱虹羁瞿雎峤矍迥岬觊睛贱甲巾峻赆囝崛岌虮尖近舰斤间景皎臼敫晶舅姐较姣嫁嫉妓辑娟妗戒婕姬皆娇轿匠奸婧巨件稼积箭矩榘箕犄嵇稽矫稷犋笕简笺秸籍笄犍季毽筋笈笳境吉鉴颉监趄坚紧赳均圾嘉劫劼即胶孓阶孑亟胛脚际肼阱腈腱胫卺降肌几久及狡饺鲛鲸鲚獍句灸咎狷桀獗馑狙飓祭饯鲒鲣急觖九鲫角鸠解饥绝绞继缉缣绢缄缰缙骄缴畿绩结骥剿纪骏经纠级驹绛金镜铰镓锩韭铗镢钜锦镌冀遽龃钾锏键锯乩锔钧加袈架迦驾")
    ("i" . "左丈殖仄砸臧斫砟逐砖砧磔粥咫展张昼帚桩梓樟柱榨植棕枝樽枳杖柘楂桎株椎柞栀桢栈榛棹枕槠杼札在注滓漳渚汁洲澡浞滋滞涿湛浙渣洙浊渍沾溱浈治涨濯淄泽沼潴著蔗藻苎菹葬蘸茱蕞芷蓁茈芝蒸荮茁煮枣斟醉鬃职真哉载栽甄酎正再整政酢遭者直髭赜支鄹翥酌酯专璋珍琢珠瓒责赘盏奏耔族章瘴装麈疰座卒粢咨疹状瘃症震斋准痄彰旃资这遮衷庄站壮痔痣恣戆鄣姿妆鹧瘵只吱咤啫踪嘲躁噪唑踯嗞咂啄跖哳啭咱踬咋踵唣躜吒躅嘬嘴趾啧呲嘱咒啁作值伫住隹做僮众仗佐侄掌侏仲俎闸侧坐债侦倬仔偬仉指摘摭拄撞哲撙捉掷征找揸振折抓攥攒挚蛰贽蜇拽徵揍择撰搌拯擢摺拙絷扎拶招执鸷挣之诸谪谆禛谘褚祝禚诊证冢诼谮诹诛翟诈祚祖蚤诅祯祉肇褶诌祗谵诏鸩诤总粽炷燥尊甑糟炸豸遵烛着州粘灶籽孳郑兹兆灼糌繇字宰忮灾憎忪怔寨窒窀窄怍宅宙惴宗中置幛嶂贮蛀幢蟑赃赠帜赚畛罾嵫赈贼崭蛭帐账帙蛛颛蜘蚱眨盅崽罪罩峙帧帻眦忠胄瞩助瞻赒睁峥足质舟爪舴舯舳卮自照曌早皂昃齄最昨追昭至辙嫜轸轵錾辄轾致斩暂轴妯匝转臻轧辎郅屯姊制朱稹箸知乍箴重簪筑籀竹稚秩笊雉笮造赞迮智纂租种毡竺箦篆怎邾乇笫秭筝则赭埴趑赵黹增凿圳趱螫址走喆志子脏障膣腙肢朕胗阵肘奘肫陬胀孜胙阼臜阻肿坠陟骘脂胝主猪獐詹炙鳟觯杂橥鲰旨昝螽周祭争馔鲻邹皱狰组综缜驻绽织纵缯骤纣缒骓彘缵驵甾缀缁纸绉驺战针镇訾镞锗卓占锃止桌砦钲锥铢锺钟贞赀铡镯钻铸钊龇紫锱铮觜终召")
    ("h" . "还灰磺瓠厚虺夯孬弧弘憨核杭横桓槐桦桁槲活湖瀚濠浣沆浍河洪潢洹汇浩淮湟海画涸洄混溷灏滑漶汗滹汉浑沪浒涵泓翮鸿涣花荒蒿菏葫荟蕙蕻藿荤薨荷萑蘅茴劐菡荭获薅醐翰酣醢卉黄鬟韩胡惠邯鹕瑚耠环砉璜魂顸珩耗琥慧彗珲邗装亥亨豪冱癀毫霍颃麾瘊痕肓痪劾号哼咳嚎嗨嗐吭呼呵喊咴哄嚯哗嚆嗥嗬吽喉喝嘿踝唬吼哏喙唤唿候阂华伙合含闳何颔颌货盒阖会辉侯阍换撼攉徨徽徊擐捍护很挥撖衡话祜戽祸扈诃诙讧户谎诲褐罕讳袆诨欢鹤火糊煳烩焓烀烘豢煌糇焊焕貉害寒怙豁怀恢恒黉宏憾宦慌惶悔悍逭寰恍恨惚鲎回蝴骸黑岵蛤虹蚶蟥贿蟪蝗骺蠖蚝幌蛔圜患囫哈后航逅音晗晦颢皓遑皇昊旱鼾晃晖毁曷好轷或互嬛划轰惑婚和禾氦簧篁篌秽鹄笏乎壕坏堠盍赫壶郝恚觳不孩胲隳隍函将猢斛訇鲩凰狐獾鹱鳇猴昏奂馄猾忽狠红骇缓绘骅绗纥缳缋幻站镐钬铪锾镬铧壑虎锪化贺")
    ("g" . "磙尬龚顾硅尴矸感硌敢艮改弓根槁椁棺桧柜梗杠柑梏栝槔棍桂杆桄概橄构枸滚沽涫涡淦鬲港泔灌汩亘滑溉澉沟格莞苷藁葛菰茛苟菇鹳哥辜更酤共耿古甘工攻聒革贡过汞贾恭鞲酐丐嘏歌巩鸪干珙瑰规遘觏耕珪高庋衮疳广疙痼赓裹庚戆膏郭赣跟咕嘎哽嗝呱噶呙剐咣哏咯跪个估倌供傀仡闺刽阁佝鸽光一搞擀拐搿拱扛掴挂搁掼该诂诟雇诰观褂癸冠诖袼诡关羔谷糕羹馘盖虢炔公怪宫寡官割惯宄国固赅蛄崞罟冈蛤蚣罡虼贵崮蜾岗蛊帼蝈刿骨骼岣鹘购故尜归形瓜舸舡果炅皋杲皈盥睾旰鬼躬晷姑戈轱妫匦匮辊媾轧轨管稿篙牯告缸筻箍罐牿刮篝竿秆乖郜簋鹄笱鸹刚鼓垓埚埂塥坩垢圪瞽赶卦圭彀毂云脘陔隔膈肛肱孤胍肝臌胱股胳各馆犷鲠鳜觚逛猓鲴旮鳏龟觥鲑勾够狗鲧给缟绀绠纥缑贯纲绲钢镐钴锅钙镉锆锢钆钩铬功哿戤尕")
    ("f" . "否奋砝砩矾砜费弗艴枫枋焚桴棼樊榧梵法涪浮瀵滏汾泛副沣沸淝散范藩芳芬蕃莩芾苻茯蘩菲芙葑乏菔整甫酚覆敷夫玢璠丰麸奉珐酆放府方腐霏痱冯邡疯废事吩蹯吠咐跗趺呋啡唪呒走仿俯份俘付伏伐傅偾阀俸垡佛下彷拊扶抚拂房访福祓袱扉讣诽讽真粉父燔粪烦斧番釜翻忿孚郛烽分是愤富悱怫直幡蜉幅罘蝠赙贩蝮畈赋蚨幞罚峰帆蜂反舫返者阜鼢妇妨辅辐匪妃教氛稃篚符筏馥复缶繁氟坟坊封黼黻赴期防腑肪肺脯附腹肤腓肥服风凡鲂鲼孵匐饭鲋鳆负逢鲱凤狒犯凫纷纺缚绂驸绯绋缝非斐钫蜚翡镄锋钒飞发")
    ("e" . "＠％≠×÷、の√〓￡?∏℡°＝≥＞〖※【●＋∠－±（◆◇：$…”—㎡π‰￥.℃△▲！﹡“？☆★∞～＊φΣ＜≤§□㈱")
    ("d" . "达歹碇礅碲殚碟夺耷砘碓碘硐碡殆磴碉砥砀导屌刁逮殿椟栋椴杜档棣柢淡滴渎滇淀渡豆沌洞逗澄凼涤澹荡菪蒂荳董蔸芏荻萏带酊鞑耋耵顶颠戴靼聃蠹觌丁耽都动靛焘玎玳纛玷毒大癫瘅冻痘疔敦瘩帝端疸盗店度憝癜底凋嘟嘀啶踮踱蹲啖蹈嗲呆喋蹀哒叮吨跌嘚吊蹬叼噔咄跺咚哚叨哆当袋代仃黛贷佃侗岱傣低儋党打抖掂德掸搭担得掉垫待挡掇抵捣对读谛登谍订诞翟褡叠迨祷裆谠怠怼邓裰凳诋道兑籴灯奠炖煅递单弟郸爹羝貂定斗窦惇惦惮惰宕懂恫怛悼忉但赌蚪黩巅睹典赕睇盹岽蝶盯囤迪峒髑帱瞪嶝鼎电眈骶断盾舵瓞遁段牍旦戥牒躲的兜倒嫡东娣趸甙顿妲到妒鸫等犊答氮稻簖箪短簟敌氘篼笛迭笪竺丢笃簦第氡地堵墩堞垤堆堤垌坫埭垛坻队腚胨胆蛋胴肚阽堕陡独冬氐狄丹朵鲽雕岛甸剁邸鲷多缎缔驮缍绐骀绦点锭镝镦镀铞铫钉钝铤铥锻督钿锝铛铎镫钓刀")
    ("c" . "存成唇磋蹙磁碴厨厝砗厂礤盛殂厕残辰础碜层尺羼迟弛孱楚楮榇榱檫槎枞材村橱槽楂樗杵椎枨棰槌查樘椿楱橙杈椽郴柽沉淳淬澈澶淙潮豉沧漕测涔池汊滁澄沈潺草茨萃蔟茺茶苁菜苍藏蒇茬苌茌菖莼蔡葱才寸裁矗醇酬酲聪醋敕酢曹刺耻朝翅琮舂璁春蚕璀蠢璨耖亍琛次瘁啻卒瘥痤疮床瓷颤痴产冲廛畜衰凑瘛疵瘳充鹑吃嗔啐蹴蹿躔嘲嚓踩蹉蹭噌喳蹰嘈躇踟唱逞喘嘬踹吵踔呈踌哧嗤啜吹叱从裳侪促阐伧丛敞伥氅倡催阊侧常创传俦偿尝俶储伺闯雠仓仇傺侈持搐撤撺擦操搓挫踅彻措拆搽捶插攒搋抽揣徂撮摧抻扯徜撑抄惩掺搀词翠叉诧褚谶禅裎诚衬戳谌参褫昶祠衩初谗谄皴差粹炽糍采豺彩糙粗汆肏灿岔刹焯炒慈冁郗炊鹚鬯重悴憧怆穿宠忖怵惭宸憷窗怅忏窜忡恻惝惨忱察怊惆曾幢虫螭崇睬嵯蝉蜍岑财螬崔瞅赐遄串畴帱蝽瞠黜黪蛏畅蟾眵尘船斥艟舱艚晨魑晁臭晟爨昌川匙囱车姹婵臣虿娼嫦辏辍媸程辞长簇矬策垂秤掣毳氚篪篡乘筹愁笞称稠城趁埕赤坼墀超坻出承膪除塍脞陈丑丞陲蚩腠屮肠脆处匆猝馇册猹舛雏饬触猖鲳猜刍馋鸱纯缠骢巢骋绰剿驰骖绌缀骣绸此铳铲镩镲餐粲鹾龊锉齿错柴铖锤雌锸钏锄觇钞铛钗龀场")
    ("b" . "布奔不磅碚殡碑礴砭泵碥甭飙夯避辟襞屏屄鐾弼檗擘壁璧剥臂嬖己榜槟杯柄板柏彬标棒梆杓滨汴渤灞泊浜百滗濞逼瀑濒波展蒡薄苄荸菠蔽蕃苯荜菝茇蓓萆葆蓖芭薜苞博丙鬓鞴本鞭髟贲逋醭靶孛邴勃鹁帮斑班碧邦耙玻部辨病瓿瘭霸庇褒瓣庳瘪亳瘢冰痹禀卞癍斌辩变辫疤雹别啵踣叭哺哔跸跋吡蹦呗嘣卟鄙跛趵便倍卜傍傧保煲伴闭佰伯俾堡把摈拌扒播捌扮拔捕搏搬捭扳摆摒报抱彼拨被谤必弁畚裨褓遍扁补褙裱褊半八焙蹩弊炳敝颁爆迸坌爸憋煸粑豹鳖炮并悖忭怖宾愎窆宝贝髌豳畀败睥髀贬罢蚌崩蝙岜般舭舶舨白卑兵暴晡魃版鼻帛币鹎比毕毙妣婢婊匾毖笔簿秉箔掰簸筚笨秕篦稗箅拜笆笾表埔甏埠坂坝吧膀膑脖陛膊膘阪巴胞孢包饽饼飚狴鳔鲅狈备刨匕惫鳊饱鸨鲍飑编缤驳绊骠缏绑绷北镑镳镔辈钚锛钵钸镖钹彪铂钣钡步背悲铋邶钯龅边办"))
  "字符汉字对应关系表")


(defun lim-find-regexp (count char fwd)
  "根据输入的字符，及生成的码表首字母对应关系表，进行查找并定位到其位置"
  (let ((lim-char-cn
         (cdr (assoc (char-to-string char) lim--char-cn-lib)))
        (lim-bound
         (unless evil-cross-lines (if fwd (line-end-position) (line-beginning-position)))))
    (setq lim-char-cn
          (cdr (assoc
                (char-to-string char)
                lim--char-cn-lib)))
    (if lim-char-cn
        (re-search-forward (format "[%c%s]" char lim-char-cn) lim-bound t count)
      (search-forward (char-to-string char) lim-bound t count))))

(defun lim-evil-find-char (count char)
  "根据count的正负及大小向对应方向查词，并返回查询结果
若count为正或nil，则向前查词，反之向后；
若查找到词，则将光标(evil模式下的光标块)置于对应的字符上
"
  ;; :type inclusive
  (interactive)
  ;; elisp只有nil
  (setq count (or count 1))
  (let ((fwd (> count 0))
        result)
    (when fwd (forward-char))
    (setq case-fold-search nil)
    (if (lim-find-regexp count char fwd)
        (setq result t)
      (setq result nil))
    (if fwd (backward-char))
    (or result
        (user-error "Can’t find: %c" char))))

(evil-define-motion lim-evil-find-char-forward (count char)
  "Search the word forward as f"
  :type inclusive
  (interactive "<c><C>")
  (setq lim-evil-last-char char)
  (setq lim-evil-last-call 'lim-evil-find-char-forward)
  (lim-evil-find-char count char))

(evil-define-motion lim-evil-find-char-forward-to (count char)
  "Search the word forward as t"
  :type inclusive
  (interactive "<c><C>")
  (setq lim-evil-last-char char)
  (setq lim-evil-last-call 'lim-evil-find-char-forward-to)
  (and
   (lim-evil-find-char count char)
   (backward-char)))

(evil-define-motion lim-evil-find-char-backword (count char)
  "Search the word backword as F"
  :type inclusive
  (interactive "<c><C>")
  (setq lim-evil-last-char char)
  (setq lim-evil-last-call 'lim-evil-find-char-backword)
  ;; 由于向后查找，count为nil时需要处理
  (lim-evil-find-char (- (or count 1)) char))

(evil-define-motion lim-evil-find-char-backword-to (count char)
  "Search the word backword as T"
  :type inclusive
  (interactive "<c><C>")
  (setq lim-evil-last-char char)
  (setq lim-evil-last-call 'lim-evil-find-char-backword-to)
  ;; 由于向后查找，count为nil时需要处理
  (and
   (lim-evil-find-char (- (or count 1)) char)
   (forward-char)))

(evil-define-motion lim-evil-find-char-repeat (count)
  "Search again as ;"
  :type inclusive
  (funcall
   lim-evil-last-call
   count lim-evil-last-char))

(evil-define-motion lim-evil-find-char-reverse (count)
  "Search again as ,"
  :type inclusive
  (cond
   ((eq lim-evil-last-call 'lim-evil-find-char-forward)
    (lim-evil-find-char-backword count lim-evil-last-char))

   ((eq lim-evil-last-call 'lim-evil-find-char-backword)
    (lim-evil-find-char-forward count lim-evil-last-char))

   ((eq lim-evil-last-call 'lim-evil-find-char-forward-to)
    (lim-evil-find-char-backword-to count lim-evil-last-char))

   ((eq lim-evil-last-call 'lim-evil-find-char-backword-to)
    (lim-evil-find-char-forward-to count lim-evil-last-char))))


(define-minor-mode lim-evil-find-mode
  "Minor mode to make Evil's f/F/t/T be able to find Chinese by lim."
  :global t
  ;; :lighter " LEFC"
  (if lim-evil-find-mode
      (progn
        (define-key
          evil-motion-state-map [remap evil-find-char]
          'lim-evil-find-char-forward)

        (define-key
          evil-motion-state-map [remap evil-find-char-backward]
          'lim-evil-find-char-backword)

        (define-key
          evil-motion-state-map [remap evil-find-char-to]
          'lim-evil-find-char-forward-to)

        (define-key
          evil-motion-state-map [remap evil-find-char-to-backward]
          'lim-evil-find-char-backword-to)

        (define-key
          evil-motion-state-map [remap evil-repeat-find-char]
          'lim-evil-find-char-repeat)
        (define-key
          evil-motion-state-map [remap evil-repeat-find-char-reverse]
          'lim-evil-find-char-reverse))

    (define-key evil-motion-state-map [remap evil-find-char]                nil)
    (define-key evil-motion-state-map [remap evil-find-char-to]             nil)
    (define-key evil-motion-state-map [remap evil-find-char-backward]       nil)
    (define-key evil-motion-state-map [remap evil-find-char-to-backward]    nil)
    (define-key evil-motion-state-map [remap evil-repeat-find-char]         nil)
    (define-key evil-motion-state-map [remap evil-repeat-find-char-reverse] nil)))


;; 2. count chinese words
(defvar lim-cn-shape  "[，。／÷？；：、＼·｜§¦｀～！＠☯＃⌘％°￥$€£¥¢¤₩……＆＊·・×※❂（）\
－——＋＝々〃‘’“”《〈«‹》〉»›「【〔［」】〕］『〖｛』〗｝]")


(defun count-chinese-words (start end)
  "Count Chinese words between START and END."
  (let ((words 0)
        (puncs 0)
        (cn-re
         ;; (rx (category chinese))
         (rx (category chinese-two-byte))))
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (while (re-search-forward
                cn-re
                nil t)
          (setq words (1+ words)))
        (goto-char (point-min))
        (while (re-search-forward
                lim-cn-shape
                nil t)
          (setq puncs (1+ puncs)))))
    (setq chinese-puncs puncs)
    (setq chinese-words words))
  (message "中文字数：%s （不计标点符号）\n中文标点：%s" chinese-words chinese-puncs))


(defun lim-count-words (start end)
  "Count Chinese words between START and END.
If called interactively, START and END are normally the start and
end of the buffer; but if the region is active, START and END are
the start and end of the region.  Print a message reporting the
number of lines, words, and chars."
  (interactive (list nil nil))
  (cond ((use-region-p)
         (count-chinese-words (region-beginning) (region-end)))
        (t
         (count-chinese-words (point-min) (point-max)))))

(defun lim-translate-string (code)
  (let ((code-list (string-to-list code))
        current-str unread-char
        total-result optional-result
        current-word possible-char)
    (toggle-input-method)
    (while (car code-list)
      (setq unread-char (char-to-string (car code-list)))
      (setq code-list (cdr code-list))

      (if possible-char
          (if (and (or (member unread-char possible-char)
                       ;; xixi rule
                       (= (length current-str) 1))
                   (member (string-to-char unread-char) lim-total-char))
              (setq current-str (concat current-str unread-char))
            (setq total-result (concat total-result current-word)
                  current-word nil
                  possible-char nil
                  current-str unread-char))
        (setq current-str (concat current-str unread-char)))

      (setq optional-result (lim-get current-str)
            current-word (caar optional-result)
            possible-char (cdr (assoc "completions" optional-result)))

      (unless current-word
        (setq total-result (concat total-result current-str)
              current-str nil)))
    (toggle-input-method)
    (setq total-result (concat total-result current-word))))


(defun lim-translate-string-in-region ()
  (interactive)
  (if (region-active-p)
      (let (result)
        (setq result (lim-translate-string
                      (buffer-substring-no-properties (region-beginning) (region-end))))
        (message (format "result: %s" result))
        (pop-mark))))

(defun lim-orderless-regexp (orig_func component)
  (let ((result (funcall orig_func component)))
    (format "%s\\|%s" result (lim-translate-string result))))

(provide 'lim-tools)
