mkdir -p bovada
wget https://sports.bovada.lv/baseball/mlb \
     -O `date +bovada/%Y-%m-%d-%H%M-moneyline.html.gz`
