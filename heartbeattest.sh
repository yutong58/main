TEST1="5954e29b-01ee-4074-8949-8415c0cbe2c9"
TEST2="5954e73e-a3ec-4fc5-bf61-df0895bad335"
WAIT=60
AMOUNT=0.01
webhook_url="http://slack.com/api/chat.postMessage"
channel=G63TFTZQX
yourapikey="n5FTGPkGdyta50FuFNg9Nwhikc2yOLQc#trZuuFStJ3t2FzmocFkRs1rcB9Z7GLY6ef8EyXm8FHBG6b5mHBHoqQGL0RfPV64O"

timestamp() {
  date +"%Y-%m-%d %H:%M:%S"
}

 while true; do
   if test $(grep $(date +%Y-%m-%d) holiday.txt -ic) -ne 1; then
    if (($(date +%u) <= 6));then
# check ledger balance before transaction
  AMOUNTBEFORE=$(curl --silent -X GET --header 'Accept: application/json' \
  --header "Authorization: API-Key ${yourapikey}" "https://play.railsbank.com/v1/customer/ledgers/${TEST1}" \
  | jq --raw-output '.amount')

  #transaction from test ledger 1 to 2 and get responce code
  status_code=$(curl --request POST --silent --write-out "\n%{http_code}\n"  --header 'Content-Type: application/json' --header 'Accept: application/json' --header "Authorization: API-Key $yourapikey" --data '{ "ledger_from_id": "'$TEST1'", "ledger_to_id": "'$TEST2'", "amount": "0.01"}' 'https://play.railsbank.com/v1/customer/transactions/inter-ledger' | sed -n '2p')

# "Railsbank Production has failed Heartbeat test"
  # send sms if failed
  # https://hooks.slack.com/services/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
  if (($status_code != 200))
  then
    sms_url="https://hooks.slack.com/services/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"
    sms_text="Railsbank Production has failed Heartbeat test
    "
    escapedText=$(echo $sms_text | sed 's/"/\"/g' | sed "s/'/\'/g" )
    json="{\"text\": \"$escapedText\"}"
    curl -s -d "payload=$json" "$sms_url"
  fi

  #wait for 1 min
  sleep $WAIT

  #check ledger balance after transaction
  AMOUNTAFTER=$(curl --silent -X GET --header 'Accept: application/json' \
  --header "Authorization: API-Key ${yourapikey}" "https://play.railsbank.com/v1/customer/ledgers/${TEST1}" \
  | jq --raw-output '.amount')

 #check the amount and generate text message send to slack
   TIME=$(timestamp)
   RIGHTAMOUNT=$(echo "$AMOUNTBEFORE - 0.01" | bc)
   if ((`bc <<< "$RIGHTAMOUNT!=$AMOUNTAFTER"`))
   then
    TEXT="wrong amount. before: EUR ${AMOUNTBEFORE}, after: EUR ${AMOUNTAFTER}, should be: EUR ${RIGHTAMOUNT}\n"
    else
      TEXT="Number Five Alive :-) EUR $AMOUNT $TIME"
    fi

    #send to slack channel
    webhook_url="https://hooks.slack.com/services/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"
    escapedText=$(echo $TEXT | sed 's/"/\"/g' | sed "s/'/\'/g" )
    json="{\"text\": \"$escapedText\"}"
    curl -s --silent -d "payload=$json" "$webhook_url"

#if test1 ledger run out of money, switch account
 if ((`bc <<< "$AMOUNTAFTER<=0.01"`))
 then
   TEMP=$TEST2
   TEST2=$TEST1
   TEST1=$TEMP
 fi
fi
fi
done
