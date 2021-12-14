rg ".\t[a-z]{1,4}\t\d{0,10}" sbfd.dict.yaml | awk -F"\t" '{print $2 " " $1}' > lim-sbfd.txt
