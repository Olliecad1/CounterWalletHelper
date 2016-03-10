### CounterWalletHelper.py is a Python script that if you have lost your public and private keys you can get them back with using counterwallethelper.py and by using your 
### passphrase you got given when signed up to counterwallet 
### CounterWalletHelper On Linux
 
1. Install a linux OS (This was tested on a raspberry pi 2 with jessie os )
2. You may have to update your os before doing this im not sure since i dont know what os your would be using all that is required is python 2.x and counterwallethelper.py 
3. install it at this link https://github.com/Olliecad1/CounterWalletHelper/archive/master.zip or install wget sudo apt-get install wget python-pip -y
4. or they other way to Download CounterWalletHelper.py wget http://github.com/Olliecad1/CounterWalletHelper/CounterWalletHelper.py
5. install missing Python packages
####sudo pip install bip32utils
####sudo pip install ecdsa
6. Now get your pass phrase and run the following commands
python CounterWalletHelper.py -h
7. now to get a list of your addresses and their private keys USE YOUR OWN PRIVATE PASS PHRASE (The pass phrase used below is just an example).
python CounterWalletHelper.py wallet --pass-phrase "then here would go your pass phrase"
This isn't a code to type in but you will have to wait for maybe about 5 seconds
and you will see this 
address: then here would be your address
you will get 3 addresses
8. If you want to dump private keys for now then use the code above the python counterwallethelper.py one 
9. But if you want to see your private keys then run these commands
python CounterWalletHelper.py --pass-phrase "Then your pass phrase goes here" --show-private
10 Then you will get your addresses and private keys
11 I will do another one on how to do it on windows and will link it to this README.md when i have done
