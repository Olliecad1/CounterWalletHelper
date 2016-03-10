import argparse 
import math, struct
from bip32utils.BIP32Key import *
from binascii import unhexlify
from hashlib import sha256
from ecdsa.ecdsa import int_to_string
mnemonic_word_list = ["like","just","love","know","never","want","time","out","there","make","look","eye","down","only","think","heart","back","then","into","about","more","away","still","them","take","thing","even","through","long","always","world","too","friend","tell","try","hand","thought","over","here","other","need","smile","again","much","cry","been","night","ever","little","said","end","some","those","around","mind","people","girl","leave","dream","left","turn","myself","give","nothing","really","off","before","something","find","walk","wish","good","once","place","ask","stop","keep","watch","seem","everything","wait","got","yet","made","remember","start","alone","run","hope","maybe","believe","body","hate","after","close","talk","stand","own","each","hurt","help","home","god","soul","new","many","two","inside","should","true","first","fear","mean","better","play","another","gone","change","use","wonder","someone","hair","cold","open","best","any","behind","happen","water","dark","laugh","stay","forever","name","work","show","sky","break","came","deep","door","put","black","together","upon","happy","such","great","white","matter","fill","past","please","burn","cause","enough","touch","moment","soon","voice","scream","anything","stare","sound","red","everyone","hide","kiss","truth","death","beautiful","mine","blood","broken","very","pass","next","forget","tree","wrong","air","mother","understand","lip","hit","wall","memory","sleep","free","high","realize","school","might","skin","sweet","perfect","blue","kill","breath","dance","against","fly","between","grow","strong","under","listen","bring","sometimes","speak","pull","person","become","family","begin","ground","real","small","father","sure","feet","rest","young","finally","land","across","today","different","guy","line","fire","reason","reach","second","slowly","write","eat","smell","mouth","step","learn","three","floor","promise","breathe","darkness","push","earth","guess","save","song","above","along","both","color","house","almost","sorry","anymore","brother","okay","dear","game","fade","already","apart","warm","beauty","heard","notice","question","shine","began","piece","whole","shadow","secret","street","within","finger","point","morning","whisper","child","moon","green","story","glass","kid","silence","since","soft","yourself","empty","shall","angel","answer","baby","bright","dad","path","worry","hour","drop","follow","power","war","half","flow","heaven","act","chance","fact","least","tired","children","near","quite","afraid","rise","sea","taste","window","cover","nice","trust","lot","sad","cool","force","peace","return","blind","easy","ready","roll","rose","drive","held","music","beneath","hang","mom","paint","emotion","quiet","clear","cloud","few","pretty","bird","outside","paper","picture","front","rock","simple","anyone","meant","reality","road","sense","waste","bit","leaf","thank","happiness","meet","men","smoke","truly","decide","self","age","book","form","alive","carry","escape","damn","instead","able","ice","minute","throw","catch","leg","ring","course","goodbye","lead","poem","sick","corner","desire","known","problem","remind","shoulder","suppose","toward","wave","drink","jump","woman","pretend","sister","week","human","joy","crack","grey","pray","surprise","dry","knee","less","search","bleed","caught","clean","embrace","future","king","son","sorrow","chest","hug","remain","sat","worth","blow","daddy","final","parent","tight","also","create","lonely","safe","cross","dress","evil","silent","bone","fate","perhaps","anger","class","scar","snow","tiny","tonight","continue","control","dog","edge","mirror","month","suddenly","comfort","given","loud","quickly","gaze","plan","rush","stone","town","battle","ignore","spirit","stood","stupid","yours","brown","build","dust","hey","kept","pay","phone","twist","although","ball","beyond","hidden","nose","taken","fail","float","pure","somehow","wash","wrap","angry","cheek","creature","forgotten","heat","rip","single","space","special","weak","whatever","yell","anyway","blame","job","choose","country","curse","drift","echo","figure","grew","laughter","neck","suffer","worse","yeah","disappear","foot","forward","knife","mess","somewhere","stomach","storm","beg","idea","lift","offer","breeze","field","five","often","simply","stuck","win","allow","confuse","enjoy","except","flower","seek","strength","calm","grin","gun","heavy","hill","large","ocean","shoe","sigh","straight","summer","tongue","accept","crazy","everyday","exist","grass","mistake","sent","shut","surround","table","ache","brain","destroy","heal","nature","shout","sign","stain","choice","doubt","glance","glow","mountain","queen","stranger","throat","tomorrow","city","either","fish","flame","rather","shape","spin","spread","ash","distance","finish","image","imagine","important","nobody","shatter","warmth","became","feed","flesh","funny","lust","shirt","trouble","yellow","attention","bare","bite","money","protect","amaze","appear","born","choke","completely","daughter","fresh","friendship","gentle","probably","six","deserve","expect","grab","middle","nightmare","river","thousand","weight","worst","wound","barely","bottle","cream","regret","relationship","stick","test","crush","endless","fault","itself","rule","spill","art","circle","join","kick","mask","master","passion","quick","raise","smooth","unless","wander","actually","broke","chair","deal","favorite","gift","note","number","sweat","box","chill","clothes","lady","mark","park","poor","sadness","tie","animal","belong","brush","consume","dawn","forest","innocent","pen","pride","stream","thick","clay","complete","count","draw","faith","press","silver","struggle","surface","taught","teach","wet","bless","chase","climb","enter","letter","melt","metal","movie","stretch","swing","vision","wife","beside","crash","forgot","guide","haunt","joke","knock","plant","pour","prove","reveal","steal","stuff","trip","wood","wrist","bother","bottom","crawl","crowd","fix","forgive","frown","grace","loose","lucky","party","release","surely","survive","teacher","gently","grip","speed","suicide","travel","treat","vein","written","cage","chain","conversation","date","enemy","however","interest","million","page","pink","proud","sway","themselves","winter","church","cruel","cup","demon","experience","freedom","pair","pop","purpose","respect","shoot","softly","state","strange","bar","birth","curl","dirt","excuse","lord","lovely","monster","order","pack","pants","pool","scene","seven","shame","slide","ugly","among","blade","blonde","closet","creek","deny","drug","eternity","gain","grade","handle","key","linger","pale","prepare","swallow","swim","tremble","wheel","won","cast","cigarette","claim","college","direction","dirty","gather","ghost","hundred","loss","lung","orange","present","swear","swirl","twice","wild","bitter","blanket","doctor","everywhere","flash","grown","knowledge","numb","pressure","radio","repeat","ruin","spend","unknown","buy","clock","devil","early","false","fantasy","pound","precious","refuse","sheet","teeth","welcome","add","ahead","block","bury","caress","content","depth","despite","distant","marry","purple","threw","whenever","bomb","dull","easily","grasp","hospital","innocence","normal","receive","reply","rhyme","shade","someday","sword","toe","visit","asleep","bought","center","consider","flat","hero","history","ink","insane","muscle","mystery","pocket","reflection","shove","silently","smart","soldier","spot","stress","train","type","view","whether","bus","energy","explain","holy","hunger","inch","magic","mix","noise","nowhere","prayer","presence","shock","snap","spider","study","thunder","trail","admit","agree","bag","bang","bound","butterfly","cute","exactly","explode","familiar","fold","further","pierce","reflect","scent","selfish","sharp","sink","spring","stumble","universe","weep","women","wonderful","action","ancient","attempt","avoid","birthday","branch","chocolate","core","depress","drunk","especially","focus","fruit","honest","match","palm","perfectly","pillow","pity","poison","roar","shift","slightly","thump","truck","tune","twenty","unable","wipe","wrote","coat","constant","dinner","drove","egg","eternal","flight","flood","frame","freak","gasp","glad","hollow","motion","peer","plastic","root","screen","season","sting","strike","team","unlike","victim","volume","warn","weird","attack","await","awake","built","charm","crave","despair","fought","grant","grief","horse","limit","message","ripple","sanity","scatter","serve","split","string","trick","annoy","blur","boat","brave","clearly","cling","connect","fist","forth","imagination","iron","jock","judge","lesson","milk","misery","nail","naked","ourselves","poet","possible","princess","sail","size","snake","society","stroke","torture","toss","trace","wise","bloom","bullet","cell","check","cost","darling","during","footstep","fragile","hallway","hardly","horizon","invisible","journey","midnight","mud","nod","pause","relax","shiver","sudden","value","youth","abuse","admire","blink","breast","bruise","constantly","couple","creep","curve","difference","dumb","emptiness","gotta","honor","plain","planet","recall","rub","ship","slam","soar","somebody","tightly","weather","adore","approach","bond","bread","burst","candle","coffee","cousin","crime","desert","flutter","frozen","grand","heel","hello","language","level","movement","pleasure","powerful","random","rhythm","settle","silly","slap","sort","spoken","steel","threaten","tumble","upset","aside","awkward","bee","blank","board","button","card","carefully","complain","crap","deeply","discover","drag","dread","effort","entire","fairy","giant","gotten","greet","illusion","jeans","leap","liquid","march","mend","nervous","nine","replace","rope","spine","stole","terror","accident","apple","balance","boom","childhood","collect","demand","depression","eventually","faint","glare","goal","group","honey","kitchen","laid","limb","machine","mere","mold","murder","nerve","painful","poetry","prince","rabbit","shelter","shore","shower","soothe","stair","steady","sunlight","tangle","tease","treasure","uncle","begun","bliss","canvas","cheer","claw","clutch","commit","crimson","crystal","delight","doll","existence","express","fog","football","gay","goose","guard","hatred","illuminate","mass","math","mourn","rich","rough","skip","stir","student","style","support","thorn","tough","yard","yearn","yesterday","advice","appreciate","autumn","bank","beam","bowl","capture","carve","collapse","confusion","creation","dove","feather","girlfriend","glory","government","harsh","hop","inner","loser","moonlight","neighbor","neither","peach","pig","praise","screw","shield","shimmer","sneak","stab","subject","throughout","thrown","tower","twirl","wow","army","arrive","bathroom","bump","cease","cookie","couch","courage","dim","guilt","howl","hum","husband","insult","led","lunch","mock","mostly","natural","nearly","needle","nerd","peaceful","perfection","pile","price","remove","roam","sanctuary","serious","shiny","shook","sob","stolen","tap","vain","void","warrior","wrinkle","affection","apologize","blossom","bounce","bridge","cheap","crumble","decision","descend","desperately","dig","dot","flip","frighten","heartbeat","huge","lazy","lick","odd","opinion","process","puzzle","quietly","retreat","score","sentence","separate","situation","skill","soak","square","stray","taint","task","tide","underneath","veil","whistle","anywhere","bedroom","bid","bloody","burden","careful","compare","concern","curtain","decay","defeat","describe","double","dreamer","driver","dwell","evening","flare","flicker","grandma","guitar","harm","horrible","hungry","indeed","lace","melody","monkey","nation","object","obviously","rainbow","salt","scratch","shown","shy","stage","stun","third","tickle","useless","weakness","worship","worthless","afternoon","beard","boyfriend","bubble","busy","certain","chin","concrete","desk","diamond","doom","drawn","due","felicity","freeze","frost","garden","glide","harmony","hopefully","hunt","jealous","lightning","mama","mercy","peel","physical","position","pulse","punch","quit","rant","respond","salty","sane","satisfy","savior","sheep","slept","social","sport","tuck","utter","valley","wolf","aim","alas","alter","arrow","awaken","beaten","belief","brand","ceiling","cheese","clue","confidence","connection","daily","disguise","eager","erase","essence","everytime","expression","fan","flag","flirt","foul","fur","giggle","glorious","ignorance","law","lifeless","measure","mighty","muse","north","opposite","paradise","patience","patient","pencil","petal","plate","ponder","possibly","practice","slice","spell","stock","strife","strip","suffocate","suit","tender","tool","trade","velvet","verse","waist","witch","aunt","bench","bold","cap","certainly","click","companion","creator","dart","delicate","determine","dish","dragon","drama","drum","dude","everybody","feast","forehead","former","fright","fully","gas","hook","hurl","invite","juice","manage","moral","possess","raw","rebel","royal","scale","scary","several","slight","stubborn","swell","talent","tea","terrible","thread","torment","trickle","usually","vast","violence","weave","acid","agony","ashamed","awe","belly","blend","blush","character","cheat","common","company","coward","creak","danger","deadly","defense","define","depend","desperate","destination","dew","duck","dusty","embarrass","engine","example","explore","foe","freely","frustrate","generation","glove","guilty","health","hurry","idiot","impossible","inhale","jaw","kingdom","mention","mist","moan","mumble","mutter","observe","ode","pathetic","pattern","pie","prefer","puff","rape","rare","revenge","rude","scrape","spiral","squeeze","strain","sunset","suspend","sympathy","thigh","throne","total","unseen","weapon","weary"]

def makeSeed(phrase, words=mnemonic_word_list):
	n = len(mnemonic_word_list) 
	s = [] 
	for i in range(4):  # len(phrase) / 3 
		w1 = words.index(phrase[3*i])
		w2 = words.index(phrase[3*i + 1])
		w3 = words.index(phrase[3*i + 2])
		s.append(w1 + n*mMod(w2-w1, n) + n*n*mMod(w3 - w2, n)) 
	return s
	
def mMod(a, b): 
	return int(a  - math.floor(a/b)*b) 

def toHex(s):
	seed = '' 
	for i in s: 
		seed += ('0'*8 + format(i, 'x'))[-8:]
	return seed 

def bytesToWords(seed):
	words = {}    
	for i in range(32):     
		ps = (i*8) >> 5
		try: ls = words[ps] 
		except: ls = 0
		try: rs = int(seed[i]) << (24 - (i % 4)*8)
		except: rs = 0
		words[ps] = (ls | rs)
	return words
	
def toWordArray(words):
	b = b'' 
	for i in words:
		b += struct.pack('>i', words[i])
	return b

def createWallet(entropy):
	I = hmac.new("Bitcoin seed", entropy, hashlib.sha512).digest()
	Il, Ir = I[:32], I[32:]
	key = BIP32Key(secret=Il, chain=Ir, depth=0, index=0, fpr='\0\0\0\0', public=False)
	return key

def getAddressAccount(key, n, show_private): 
	AccountZero = key.ChildKey(BIP32_HARDEN)
	ExternalAccount = AccountZero.ChildKey(0)
	address_list = [] 
	for i in range(n):
		AddressAccount = ExternalAccount.ChildKey(i)
		x = {'address': AddressAccount.Address()} 
		if show_private:
			x['private key'] = AddressAccount.PrivateKey().encode('hex')
		address_list.append(x) 
	return address_list 

def openWallet(pass_phrase, depth, show_private=False, old=False):
	step1 = makeSeed(pass_phrase)
	step2 = toHex(step1)
        if old:
                step3 = bytesToWords(step2)
                step4 = toWordArray(step3)
	else:
                step4 = unhexlify(step2) 
        wallet = createWallet(step4) 
	address_list = getAddressAccount(wallet, depth, show_private)
	return address_list

def recoverWallet(known_phrase, known_address, insertion_index, depth=3, old=False): 
	for i in range(1626):
                try_phrase = known_phrase.lower().strip().split(' ')
		try_phrase.insert(insertion_index, mnemonic_word_list[i])
		address_list = list(x['address'] for x in openWallet(try_phrase, depth, old) )
		print(try_phrase)
		print(i, address_list)
		if known_address in address_list: 
			print("FOUND: " + mnemonic_word_list[i]) 
			break
		else: continue

def checkPhrase(phrase, partial=False):
	try: p = phrase.lower().strip().split(' ')
	except: return False 
	if not partial and len(p) != 12:
		return False 
	if partial and len(p) != 11: 
		return False
	for i in p:
		if i not in mnemonic_word_list:
			return False
	return True

_b58chars = '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz'

def decode_base58(addr, length):
	n = 0
	for char in addr:
		n = n * 58 + _b58chars.index(char)
	return (b'\0'*length + int_to_string(n))[-length:] #n.to_bytes(length, 'big')
	
def checkAddress(addr):
	addr_bytes = decode_base58(addr, 25)
	addr1 = sha256(addr_bytes[:-4]).digest()
	addr2 = sha256(addr1).digest()
	chksum = addr2[:4]
	return chksum == addr_bytes[-4:]

def main():
	parser = argparse.ArgumentParser(prog='walletHelper', description='Offline helper methods for CounterWallet (BIP32) ')
	subparsers = parser.add_subparsers(dest='action', help='the action to be taken')
	parser_recover = subparsers.add_parser('recover', help='recover a wallet from a partial passphrase') 
	parser_recover.add_argument('--known-phrase', required=True, help='enter the part of the passphrase which you know') 
	parser_recover.add_argument('--known-address', required=True, help='enter an address known to be in the wallet') 
	parser_recover.add_argument('--search-depth', required=False, help='enter the number of generated addresses to search for a match per wallet', type=int, default=3) 
	parser_recover.add_argument('--insertion-index', required=True, help='insertion point for unknown word, from 0 to 11', type=int, choices=range(0,12))
	parser_recover.add_argument('--old', required=False, help='for old 10^32 entropy counterwallets', action='store_true', default=False)
        
	parser_wallet = subparsers.add_parser('wallet', help='open a wallet, show public or private keys') 
	parser_wallet.add_argument('--pass-phrase', required=True, help='enter the wallet passphrase', )
	parser_wallet.add_argument('--search-depth', required=False, help='enter the numberas of addresses to display', type=int, default=3) 
	parser_wallet.add_argument('--show-private', required=False, help='show wallet private keys', action='store_true', default=False) 
	parser_wallet.add_argument('--old', required=False, help='for old 10^32 entropy counterwallets', action='store_true', default=False)
        args = parser.parse_args()
 
	if args.action == 'wallet':
		try: 
			assert checkPhrase(args.pass_phrase)
                        pass_phrase = args.pass_phrase.lower().strip().split(' ')
		except: 
			print('re-enter a valid 12 word pass phrase, or try using the recovery option')
			return
		depth = abs(args.search_depth)
		address_list = openWallet(pass_phrase, depth, args.show_private, args.old)
		for i in address_list: 
			print('address: %s' % i['address'])
			if args.show_private:
				print('private key: %s' % i['private key'])
				
	elif args.action == 'recover':
		try: 
			assert checkPhrase(args.known_phrase, partial=True)
		except: 
			print('make sure to enter 11 valid known mnemonic words')
			return
		try: 
			assert checkAddress(args.known_address)
		except:
			print('enter a valid known address')
			return 
		depth = abs(args.search_depth)
		recoverWallet(args.known_phrase, args.known_address, args.insertion_index, depth, args.old) 
		
if __name__ == '__main__':
	main()