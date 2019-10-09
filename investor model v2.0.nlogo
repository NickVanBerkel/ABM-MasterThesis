;;XYZ rounding of shares bought by NFCs
;;XYZ two different priorities for spending net income
;;XYZ

extensions [
  table
  matrix
  gis
]

globals [
  us-dataset
  world-patches
  investing-strategy-list
  most-successful-strategy
  most-successful-investor
  average-return-chartists
  average-return-fundamentalists
  number-of-traded-shares
  mean-initial-share-price
  stdev-initial-share-price
  list-real-investment-rate
  list-return-of-investors
  total-tax-break
  total-capital-spent-on-repurchases
  interest-table
]


breed [
  NFCs NFC
]

breed [
  investors investor
]

breed [
  nodes node
]

breed [
  orders order
]

undirected-link-breed [
  node-links node-link
]

undirected-link-breed [
  investor-links investor-link
]

NFCs-own [
  price-per-share ;;float
  market-cap ;;float - multiplication of price-per-share and number-of-outstanding-shares
  debt-to-equity-ratio ;;float
  debt ;;float - multiplication of debt-to-equity ratio and market capitalization
  interest-rate ;;float
  number-of-outstanding-shares ;;integer
  price-history ;;dictionary of timestamp as key and price per share as value
  dividend? ;;boolean
  dividend-rate ;;float
  dividend-payout ;;float
  dividend-payout-history ;;table
  dividend-payout-rate
  decision-rules ;;string
  share-repurchasing-programme? ;;boolean
  share-repurchasing-target ;;float
  capital-spent-on-repurchases ;;float
  share-repurchasing-rate
  shares-repurchased ;;integer
  shares-repurchased-history ;;table
  spendable-liquidity ;;float
  price-to-earnings-ratio ;;float
  ebit ;;integer
  ebit-with-tax-break ;;integer
  ebt ;;float
  net-income ;;integer
  net-income-history ;;dictionary of timestamp as key and net income as value
  EPS ;;float
  EPS-history ;;dictionary of timestamp as key and EPS as value
  EPS-growth-target ;;float
  growth-rate ;;float
  capital-spent-on-real-investment ;;float
  investment-history ;;float
  real-investment-rate ;;float
  average-return-of-investors ;;dictionary of timestamp as key and average return of investors as value
  percentages-payout-and-investment ;;list
  forecast-matrix ;; matrix - rows are NFCs in order - column 1 : current price of stock - column 2 : forecast price of stock - column 3 : forecast return of stock - column 4 : excess demand of stock
]

investors-own [
  portfolio ;;matrix - rows are NFCs in order - column 1 : number of owned shares - column 2 : average buying price of shares - column 3 : return on shares
  portfolio-wealth ;;float
  liquid-wealth ;;float
  total-wealth ;;float - sum of liquid-wealth and portfolio-wealth
  return-on-investment ;;float
  liquidity-demand ;;float
  spendable-liquidity ;;float
  investing-strategy ;;string - either 'chartist' or 'fundamentalist'
  fundamentalist-equilibrium-level ;;float
  fundamentalist-learning-score ;;float
  time-as-fundamentalist ;;integer
  chartist-trend-speed ;;float
  chartist-learning-score ;;float
  time-as-chartist ;;integer
  floor-level ;;dictionary of NFC as key and floor level as value
  forecast-matrix ;; matrix - rows are NFCs in order - column 1 : current price of stock - column 2 : forecast price of stock - column 3 : forecast return of stock - column 4 : excess demand of stock
  counter-strategy-switch ;;integer
]

patches-own [
  world?
]

to setup
  ca
  reset-ticks

  define-globals
  define-interest-table
  spawn-nfcs
  spawn-investors
  determine-decision-rules
  distribute-shares
  create-investors-network
  print "Model initialized."
end

to define-globals
  set mean-initial-share-price 107.77
  set investing-strategy-list [ "chartist" "fundamentalist"]
  ;; Mean and standard deviation taken from the share prices of the companies in the S&P 500 index
end

to define-interest-table
  set interest-table table:make
  let table-count 1
  foreach (range 0.1 2.1 0.1) [
    dti ->
    table:put interest-table dti minimum-interest-rate + table-count * ((maximum-interest-rate - minimum-interest-rate) / 20)
    set table-count table-count + 1
  ]
end

to spawn-nfcs
  create-NFCs number-of-NFCs [
    set hidden? true
    move-to one-of patches
    set shape "building institution"
    set size 14
    set color grey

    set price-per-share mean-initial-share-price
    set price-history table:make
    table:put price-history 0 price-per-share

    set market-cap 3e10
    set number-of-outstanding-shares round (market-cap / price-per-share)

    set price-to-earnings-ratio 0.215
    set net-income market-cap * price-to-earnings-ratio
    set net-income-history table:make
    table:put net-income-history 0 net-income
    set spendable-liquidity net-income

    set debt-to-equity-ratio 1.1
    set debt debt-to-equity-ratio * market-cap
    set interest-rate table:get interest-table round(debt-to-equity-ratio)

    set ebt (net-income / (1 - tax-profit) )
    set ebit ebt + (interest-rate * debt)

    ;;Creation of a logarithmic distribution for the dividend rate of NFCs. About 20 percent of the companies does not give out any dividend, so the stochastic variable dividend? determines if the NFC gives out dividend or not (fitted from data of S&P 500 companies)
    set dividend? true
    set dividend-rate 0.05
    set dividend-payout-history table:make
    set EPS net-income / number-of-outstanding-shares
    set EPS-history table:make
    table:put EPS-history 0 EPS
    set shares-repurchased-history table:make
    set investment-history table:make
    set capital-spent-on-real-investment 0
    set EPS-growth-target 0.02 + random-float 0.1
    set growth-rate -0.10 + random-float 0.20
    set real-investment-rate 0
    table:put investment-history 0 0
    set average-return-of-investors table:make
    set forecast-matrix matrix:make-constant count NFCs 4 0
    set percentages-payout-and-investment ( list ( percentage-of-net-income-to-payouts)  (percentage-of-net-income-to-investment))
  ]
end

to spawn-investors
  create-investors number-of-investors [
    set hidden? true
    move-to one-of patches
    set shape "person business"
    set size 9

    set liquidity-demand minimum-liquidity-demand + random-float (maximum-liquidity-demand - minimum-liquidity-demand)
    set investing-strategy one-of investing-strategy-list
    set fundamentalist-equilibrium-level random-float 1
    set fundamentalist-learning-score 1
    set time-as-fundamentalist 0
    set floor-level table:make
    set chartist-trend-speed random-float 1
    set chartist-learning-score 1
    set time-as-chartist 0
    set forecast-matrix matrix:make-constant count NFCs 4 0
  ]
end

to determine-decision-rules
  if decision-rules-NFCs = "max-shareholder-value-and-max-net-income" [
    ask NFC 0 [
      set decision-rules "max-shareholder-value"
    ]
    ask NFC 1 [
      set decision-rules "max-net-income"
    ]
  ]
  if decision-rules-NFCs = "max-shareholder-value-and-constant-payout-and-investment" [
    ask NFC 0 [
      set decision-rules "max-shareholder-value"
    ]
    ask NFC 1 [
      set decision-rules "constant-payout-and-investment"
    ]
  ]
  if decision-rules-NFCs = "constant-payout-and-investment-and-max-net-income" [
    ask NFC 0 [
      set decision-rules "constant-payout-and-investment"
    ]
    ask NFC 1 [
      set decision-rules "max-net-income"
    ]
  ]
  if decision-rules-NFCs = "only-max-shareholder-value" [
    ask NFCs [
      set decision-rules "max-shareholder-value"
    ]
  ]
  if decision-rules-NFCs = "only-max-net-income" [
    ask NFCs [
      set decision-rules "max-net-income"
    ]
  ]
  if decision-rules-NFCs = "only-constant-payout-and-investment" [
    ask NFCs [
      set decision-rules "constant-payout-and-investment"
    ]
  ]
end

to distribute-shares
  ;They equally distribute the shares over all investors, have to apply a more realistic and faster algorithm for this
  ask investors [
    set portfolio matrix:make-constant count NFCs 4 0
    foreach range count NFCs [
      firm ->
      let initial-return-share -0.1 + random-float 0.2
      if initial-return-share > 0 [
        set initial-return-share (initial-return-share * (1 - tax-capital-gain))
      ]
      matrix:set-row portfolio firm (list (round ([number-of-outstanding-shares] of NFC firm / count investors)) ([price-per-share] of NFC firm / (1 + initial-return-share)) (initial-return-share) ([price-per-share] of NFC firm))
      table:put floor-level firm [price-per-share] of NFC firm * ( 0.7 + random-float 0.6 )
    ]
  ]
  foreach range count NFCs [
    firm ->
    ask one-of investors [
      matrix:set portfolio firm 0 matrix:get portfolio firm 0 + ([number-of-outstanding-shares] of NFC firm - sum [matrix:get portfolio firm 0] of investors)
    ]
  ]
  ask investors [
    set portfolio-wealth sum (map * matrix:get-column portfolio 0 matrix:get-column portfolio 1)
    set liquid-wealth 0.5 * portfolio-wealth
    set total-wealth portfolio-wealth + liquid-wealth
    set spendable-liquidity max (list (liquid-wealth - (total-wealth * liquidity-demand)) 0)
    set return-on-investment sum (map * ( map * matrix:get-column portfolio 0 matrix:get-column portfolio 1 ) matrix:get-column portfolio 2) / sum ( map * matrix:get-column portfolio 0 matrix:get-column portfolio 1 )
  ]

  ask NFCs [
    let average-return-on-investment mean [matrix:get portfolio [who] of myself 2] of investors with [matrix:get portfolio [who] of myself 0 > 0]
    table:put average-return-of-investors 0 average-return-on-investment
  ]

end

to create-investors-network
  ask investors [
    create-investor-links-with other investors in-radius 50 [
      set hidden? true
    ]
  ]
end

to go
  calculate-forecast
  create-equilibrium
  calculate-return-and-new-price
  determine-strategy
  determine-learning-score
  if remainder ticks 10 = 0 and ticks != 0 [
    determine-cum-dividend
  ]
  if remainder ticks 12 = 0 and ticks != 0 [
    payout-dividend
    determine-investment-debt-repurchasing
    determine-new-payout-policy
  ]
  update-stats
  verification
  tick
end

to calculate-forecast
  ask investors [
    foreach range item 0 matrix:dimensions portfolio [
      firm ->
      if investing-strategy = "chartist" [
        ;; Simple compounded forecasting model for determining the next price of shares
        let current-price [price-per-share] of NFC firm
        matrix:set forecast-matrix firm 0 current-price
        let forecast-list matrix:forecast-compound-growth [table:values price-history] of NFC firm
        let forecast-price max list (current-price + chartist-trend-speed * (item 0 forecast-list - current-price)) 0.1
        matrix:set forecast-matrix firm 1 forecast-price
        let forecast-return-per-share ((forecast-price - matrix:get portfolio firm 1) * (1 - tax-capital-gain) + (([dividend-rate] of NFC firm * forecast-price) * (1 - tax-dividend))) / matrix:get portfolio firm 1
        matrix:set forecast-matrix firm 2 forecast-return-per-share
      ]

      if investing-strategy = "fundamentalist" [
        ;; Floor level can be based on cash flow, dividend rate, capital etc.
        let current-price [price-per-share] of NFC firm
        matrix:set forecast-matrix firm 0 current-price
        let forecast-price max list (current-price + fundamentalist-equilibrium-level * (table:get floor-level firm - current-price)) 0.1
        matrix:set forecast-matrix firm 1 forecast-price
        let forecast-return-per-share ((forecast-price - matrix:get portfolio firm 1) * (1 - tax-capital-gain) + (([dividend-rate] of NFC firm * forecast-price) * (1 - tax-dividend))) / matrix:get portfolio firm 1
        matrix:set forecast-matrix firm 2 forecast-return-per-share
      ]
    ]
    calculate-excess-demand
  ]

  ;; Based on excess demands, orders will be placed to buy or sell stocks
  ;; The new equilibrium price will be the price for which the sum of excess demands is equal to zero --> sum ED for n in N (Pt) = 0
end

to calculate-excess-demand
  foreach range item 0 matrix:dimensions forecast-matrix [
    firm ->
    let excess-demand round(((matrix:get forecast-matrix firm 1 - matrix:get forecast-matrix firm 0) / matrix:get forecast-matrix firm 0) * excess-demand-factor)
    matrix:set forecast-matrix firm 3 excess-demand
    ]
end

to create-equilibrium
  foreach range count NFCs [
    firm ->
    let demand-investors turtle-set nobody
    ifelse [share-repurchasing-programme?] of NFC firm = true [
      set demand-investors (turtle-set investors with [matrix:get forecast-matrix firm 3 > 0 and spendable-liquidity > [price-per-share] of NFC firm] NFC firm)
    ] [ set demand-investors investors with [matrix:get forecast-matrix firm 3 > 0 and spendable-liquidity > [price-per-share] of NFC firm] ]
    let supply-investors investors with [matrix:get forecast-matrix firm 3 < 0]
    ask supply-investors [
      let bidding-table table:make
      ask demand-investors with [matrix:get forecast-matrix firm 3 > 0 and spendable-liquidity > [price-per-share] of NFC firm] [
        table:put bidding-table who matrix:get forecast-matrix firm 1
      ]

      set bidding-table sort-table bidding-table
      let average-bid-price 0
      let number-of-sold-shares 0
      while [matrix:get forecast-matrix firm 3 != 0 and any? demand-investors = true and empty? table:to-list bidding-table = false] [
        let business-partner one-of demand-investors with [who = item 0 (item 0 table:to-list bidding-table)]
        let bid-price 1
        ifelse length table:to-list bidding-table > 1 [
          set bid-price item 1 (item 1 table:to-list bidding-table)
        ] [ if item 0 (item 0 table:to-list bidding-table) > 0 [
            set bid-price item 0 (item 0 table:to-list bidding-table)
          ]
        ]
        let bid-amount 0
        if bid-price > 0 [
          set bid-amount min (list [matrix:get forecast-matrix firm 3] of business-partner abs(matrix:get forecast-matrix firm 3) round([spendable-liquidity] of business-partner / bid-price))
        ]
        if bid-amount > 0 [
          set number-of-traded-shares number-of-traded-shares + bid-amount
          let transaction-capital bid-amount * bid-price

          set average-bid-price (average-bid-price * number-of-sold-shares + transaction-capital) / (number-of-sold-shares + bid-amount)
          set number-of-sold-shares 0 + bid-amount

          matrix:set forecast-matrix firm 3 matrix:get forecast-matrix firm 3 + bid-amount
          matrix:set portfolio firm 0 matrix:get portfolio firm 0 - bid-amount
          set liquid-wealth liquid-wealth + transaction-capital
          set spendable-liquidity spendable-liquidity + transaction-capital

          ask business-partner [
            if breed = investors [
              if (matrix:get portfolio firm 0 + bid-amount) > 0 [
                matrix:set portfolio firm 1 ((matrix:get portfolio firm 1 * matrix:get portfolio firm 0 + transaction-capital) / (matrix:get portfolio firm 0 + bid-amount))
              ]
              matrix:set portfolio firm 0 matrix:get portfolio firm 0 + bid-amount
              if matrix:get portfolio firm 1 > 0 [
                matrix:set portfolio firm 2 (([price-per-share] of NFC firm - matrix:get portfolio firm 1) / matrix:get portfolio firm 1)
              ]
              set liquid-wealth liquid-wealth - transaction-capital
            ]

            if breed = NFCs [
              set number-of-outstanding-shares number-of-outstanding-shares - bid-amount
              set capital-spent-on-repurchases capital-spent-on-repurchases + (bid-amount * bid-price)
              set shares-repurchased shares-repurchased + bid-amount
            ]

            matrix:set forecast-matrix firm 3 matrix:get forecast-matrix firm 3 - bid-amount
            set spendable-liquidity spendable-liquidity - transaction-capital
            if spendable-liquidity < item 0 (item 0 table:to-list bidding-table) [
              set demand-investors other demand-investors
            ]

            if matrix:get forecast-matrix firm 3 = 0 [
              set demand-investors other demand-investors
            ]
          ]
        ]
        table:remove bidding-table [who] of business-partner
      ]
    ]
  ]

  ask NFCs [
    set net-income spendable-liquidity
  ]
end

to calculate-return-and-new-price
  ask investors [
    ifelse sum ( map * matrix:get-column portfolio 0 matrix:get-column portfolio 1 ) != 0 [
      set return-on-investment sum (map * ( map * matrix:get-column portfolio 0 matrix:get-column portfolio 1 ) matrix:get-column portfolio 2) / sum ( map * matrix:get-column portfolio 0 matrix:get-column portfolio 1 )
    ] [ set return-on-investment 0 ]
  ]

  ask NFCs [
    let demand-minus-supply difference-demand-supply [matrix:get forecast-matrix [who] of myself 3] of investors
    if share-repurchasing-target > 0 [
      set demand-minus-supply demand-minus-supply + 1
    ]
    let new-price max ( list (price-per-share + lambda-price-adjustment * demand-minus-supply) 1)
    set price-per-share new-price
    table:put price-history ticks price-per-share
  ]

  ask investors [
    foreach range count NFCs [
      firm ->
      matrix:set portfolio firm 3 [price-per-share] of NFC firm
    ]
  ]
end

to determine-learning-score
  ask investors [
    if investing-strategy = "fundamentalist" [
      foreach range count NFCs [
        firm ->
        set time-as-fundamentalist time-as-fundamentalist + 1
        let error-fundamentalist e ^ ( (matrix:get forecast-matrix firm 1 - matrix:get portfolio firm 3) / matrix:get portfolio firm 3 )
        set fundamentalist-learning-score ( ( ( time-as-fundamentalist - 1 ) / time-as-fundamentalist ) * fundamentalist-learning-score ) + ( ( 1 / time-as-fundamentalist ) * error-fundamentalist )
        ]
      ]

    if investing-strategy = "chartist" [
      foreach range count NFCs [
        firm ->
        set time-as-chartist time-as-chartist + 1
        let error-chartist e ^ ( (matrix:get forecast-matrix firm 1 - matrix:get portfolio firm 3) / matrix:get portfolio firm 3)
        set chartist-learning-score ( ( ( time-as-chartist - 1 ) / time-as-chartist ) * chartist-learning-score ) + ( ( 1 / time-as-chartist ) * error-chartist )
      ]
    ]
  ]
end

to determine-strategy
  ask investors [
    if count link-neighbors with [investing-strategy = "chartist"] != 0 and count link-neighbors with [investing-strategy = "fundamentalist"] != 0 [
      let most-successful-investor-links max-one-of link-neighbors [return-on-investment]
      let mean-chartists 0
      let mean-fundamentalists 0

      if count link-neighbors with [investing-strategy = "chartist"] > 0 [
        set mean-chartists mean [return-on-investment] of link-neighbors with [investing-strategy = "chartist"]
      ]
      if count link-neighbors with [investing-strategy = "fundamentalist"] > 0 [
        set mean-fundamentalists mean [return-on-investment] of link-neighbors with [investing-strategy = "fundamentalist"]
      ]

      let most-successful-strategy-links ""
      ifelse mean-fundamentalists >= mean-chartists [
        set most-successful-strategy-links "fundamentalist"
      ] [ set most-successful-strategy-links "chartist" ]

      let random-strategy-factor random 2
      if random-strategy-factor = 0 [
        if investing-strategy != most-successful-strategy-links [
          if return-on-investment < mean [return-on-investment] of link-neighbors with [investing-strategy = most-successful-strategy-links] [
            set investing-strategy most-successful-strategy-links
            set counter-strategy-switch counter-strategy-switch + 1
          ]
        ]
      ]
      if random-strategy-factor = 1 [
        if investing-strategy != [investing-strategy] of most-successful-investor-links [
          set investing-strategy [investing-strategy] of most-successful-investor-links
          set counter-strategy-switch counter-strategy-switch + 1
        ]
      ]
    ]
  ]
end

to payout-dividend
  ask NFCs [
    set dividend-payout dividend-rate * (table:get price-history (ticks - 2) * number-of-outstanding-shares)
    table:put dividend-payout-history ticks dividend-payout
    set dividend-payout-rate dividend-payout / ebit
    ask investors with [matrix:get portfolio [who] of myself 0 > 0] [
      let personal-capital-cum-dividend [table:get price-history (ticks - 12)] of myself * matrix:get portfolio [who] of myself 0
      let personal-dividend personal-capital-cum-dividend * [dividend-rate] of myself
      set liquid-wealth liquid-wealth + (personal-dividend * (1 - tax-dividend))
    ]
    let ex-dividend-price price-per-share - (dividend-rate * price-per-share)
    set price-per-share ex-dividend-price
  ]
end

to determine-investment-debt-repurchasing
  ;investment rate aanpassen
  ask NFCs [
    ifelse decision-rules = "constant-payout-and-investment" [
      set capital-spent-on-real-investment net-income - (number-of-outstanding-shares * table:get price-history (ticks - 12) * dividend-rate)
      table:put investment-history ticks capital-spent-on-real-investment
      set real-investment-rate capital-spent-on-real-investment / ebit
      if real-investment-rate < percentage-of-net-income-to-investment [
        set real-investment-rate percentage-of-net-income-to-investment
        set capital-spent-on-real-investment real-investment-rate * ebit
      ]
      set net-income net-income - capital-spent-on-real-investment
      if net-income < 0 [
        set debt debt - net-income
      ]
    ]
    [set capital-spent-on-real-investment net-income - (number-of-outstanding-shares * table:get price-history (ticks - 12) * dividend-rate)
      table:put investment-history ticks capital-spent-on-real-investment
      set real-investment-rate capital-spent-on-real-investment / ebit
      set net-income net-income - capital-spent-on-real-investment
      if net-income < 0 [
        set debt debt - net-income
      ]
    ]
    set share-repurchasing-rate capital-spent-on-repurchases / ebit
    set dividend-payout-rate dividend-payout / ebit
  ]
end

to determine-new-payout-policy
  ask NFCs [
    if decision-rules = "max-shareholder-value" [
      calculate-new-income
      calculate-share-repurchasing-MSV
      calculate-dividend-rate-MSV
    ]
    if decision-rules = "max-net-income" [
      calculate-new-income
      calculate-dividend-rate-CPI
      calculate-share-repurchasing-MNI
    ]
    if decision-rules = "constant-payout-and-investment" [
      calculate-new-income
      calculate-dividend-rate-CPI
      calculate-share-repurchasing-CPI
    ]
  ]
end

to calculate-new-income
  set ebit (ebit * (1 + growth-rate)) + (table:get investment-history (ticks - 12) * return-on-real-investment-NFCs)
  set ebt ebit - (interest-rate * debt)

  ifelse ebt > 0 [
    set net-income ebt * (1 - tax-profit)
  ] [ set net-income ebt ]
  table:put net-income-history ticks net-income
  set spendable-liquidity net-income
  set EPS net-income / number-of-outstanding-shares
  table:put EPS-history ticks EPS

  set growth-rate -0.10 + random-float 0.20
end

to calculate-share-repurchasing-MSV
  let EPS-difference EPS - table:get EPS-history (ticks - 12)
  ifelse (EPS-difference / table:get EPS-history (ticks - 12)) < EPS-growth-target [
    set share-repurchasing-programme? true
    set share-repurchasing-target round (number-of-outstanding-shares - (net-income / ((1 + EPS-growth-target) * table:get EPS-history (ticks - 12))))
    matrix:set-row forecast-matrix who (list (price-per-share) (price-per-share * 1.1) (0.01) (share-repurchasing-target))
  ] [
    set share-repurchasing-programme? false
    set share-repurchasing-target 0
    matrix:set-row forecast-matrix who [0 0 0.01 0]
  ]
  set EPS-growth-target 0.02 + random-float 0.1
end

to calculate-dividend-rate-MSV
  let average-return-on-investment mean [matrix:get portfolio [who] of myself 2] of investors with [matrix:get portfolio [who] of myself 0 > 0]
  table:put average-return-of-investors ticks average-return-on-investment
  if average-return-on-investment < table:get average-return-of-investors (ticks - 12) [
    set dividend-rate dividend-rate * (1 + dividend-increase)
  ]
end

to calculate-share-repurchasing-MNI
  let net-income-difference table:get net-income-history ticks - table:get net-income-history (ticks - 12)
  ifelse net-income-difference > 0 [
    set share-repurchasing-programme? true
    set share-repurchasing-target round ( (net-income - market-cap * dividend-rate - (net-income * percentage-of-net-income-to-investment)) / price-per-share )
    matrix:set-row forecast-matrix who (list (price-per-share) (price-per-share * 1.1) (0.01) (share-repurchasing-target))
  ] [
    set share-repurchasing-programme? true
    set share-repurchasing-target round ( ( (net-income - market-cap * dividend-rate) / price-per-share ) * 0.5 )
    matrix:set-row forecast-matrix who (list (price-per-share) (price-per-share * 1.1) (0.01) (share-repurchasing-target))
  ]
end

to calculate-dividend-rate-CPI
  let net-income-difference net-income - table:get net-income-history (ticks - 12)
  if net-income-difference > 0 and table:get net-income-history (ticks - 12) > 0 [
    let net-income-increase net-income-difference / table:get net-income-history (ticks - 12)
    if net-income-increase > 0.05 [
      set dividend-rate dividend-rate * (1 + dividend-increase)
    ]
  ]
end

to calculate-share-repurchasing-CPI
  set share-repurchasing-programme? true
  set share-repurchasing-target round ( (item 0 percentages-payout-and-investment * net-income) - (dividend-rate * market-cap) / price-per-share )
  matrix:set-row forecast-matrix who (list (price-per-share) (price-per-share * 1.1) (0.01) (share-repurchasing-target))
end

to determine-cum-dividend
  ask NFCs with [dividend? = true] [
    let cum-dividend-price price-per-share + (dividend-rate * price-per-share)
    set price-per-share cum-dividend-price
  ]
end

to update-stats
  ask investors [
    set portfolio-wealth sum (map * matrix:get-column portfolio 0 matrix:get-column portfolio 3)
    set total-wealth portfolio-wealth + liquid-wealth
    set spendable-liquidity max (list (liquid-wealth - (total-wealth * liquidity-demand)) 0)
  ]

  ask NFCs [
    set total-capital-spent-on-repurchases total-capital-spent-on-repurchases + capital-spent-on-repurchases
    if remainder ticks 12 = 0 [
      table:put shares-repurchased-history ticks shares-repurchased
      set capital-spent-on-repurchases 0
      set shares-repurchased 0
    ]
    set market-cap price-per-share * number-of-outstanding-shares
  ]

  let highest-return max [return-on-investment] of investors
  set most-successful-investor investors with [return-on-investment = highest-return]

  let mean-chartists-global 0
  let mean-fundamentalists-global 0
  if count investors with [investing-strategy = "chartist"] > 0 [
    set mean-chartists-global mean [return-on-investment] of investors with [investing-strategy = "chartist"]
  ]
  if count investors with [investing-strategy = "fundamentalist"] > 0 [
    set mean-fundamentalists-global mean [return-on-investment] of investors with [investing-strategy = "fundamentalist"]
  ]
  ifelse mean-fundamentalists-global >= mean-chartists-global [
    set most-successful-strategy "fundamentalist"
  ] [ set most-successful-strategy "chartist" ]

  set list-real-investment-rate [real-investment-rate] of NFCs
  set list-return-of-investors [return-on-investment] of investors

end

to verification
  if sum [number-of-outstanding-shares] of NFCs != sum [sum matrix:get-column portfolio 0] of investors [
    print "Number of outstanding shares is not equal to the number of shares in the portfolios of the investors!"
  ]
end

to-report frequency [it li]
    report length (filter [ i -> i = it] li)
end

to-report difference-demand-supply [li]
  report length (filter [i -> i > 0] li) - length (filter [i -> i < 0] li)
end

to-report sort-table [table_to_sort]
  let sorted_table table:make
  let tempList sort-by [[?1 ?2] -> last ?1 > last ?2] table:to-list table_to_sort
  set sorted_table table:from-list tempList
  report sorted_table
end

to-report average-return-on-NFCs-with-dividend? [boolean]
  let sum-return-on-investment 0
  let number-of-inv 0
  ask NFCs with [dividend? = boolean] [
    ask investors with [matrix:get portfolio [who] of myself 0 > 0] [
      set sum-return-on-investment sum-return-on-investment + matrix:get portfolio [who] of myself 2
      set number-of-inv number-of-inv + 1
    ]
  ]
  report sum-return-on-investment / number-of-inv
end
@#$#@#$#@
GRAPHICS-WINDOW
232
24
1041
434
-1
-1
1.0
1
10
1
1
1
0
1
1
1
-400
400
-200
200
0
0
1
ticks
30.0

BUTTON
120
15
184
48
Setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
107
51
184
84
Go once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
92
87
184
120
Go forever
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
865
444
1040
477
Inspect a random NFC
inspect one-of NFCs
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
864
482
1041
515
Inspect a random investor
inspect one-of investors
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
1076
120
1513
378
Price per share of NFCs
ticks
USD/share
0.0
100.0
0.0
30.0
true
false
"" ""
PENS
"price0" 1.0 0 -16777216 true "" "plot [price-per-share] of NFC 0"
"price1" 1.0 0 -3844592 true "" "plot [price-per-share] of NFC 1"

MONITOR
1867
125
2095
170
Number of shares in investors portfolio
sum [sum (matrix:get-column portfolio 0)] of investors
17
1
11

SLIDER
22
586
194
619
tax-dividend
tax-dividend
0
0.5
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
23
624
195
657
tax-capital-gain
tax-capital-gain
0
0.5
0.5
0.01
1
NIL
HORIZONTAL

PLOT
1541
124
1845
380
Percentage of fundamentalists in the model
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot (count investors with [investing-strategy = \"fundamentalist\"] / count investors) "

TEXTBOX
23
546
209
580
Tax rates on dividends, capital gain and profits
14
0.0
1

TEXTBOX
34
132
184
157
Input variables
20
0.0
1

SLIDER
15
158
192
191
number-of-investors
number-of-investors
50
200
100.0
10
1
NIL
HORIZONTAL

SLIDER
16
193
193
226
number-of-NFCs
number-of-NFCs
2
5
2.0
1
1
NIL
HORIZONTAL

SLIDER
16
231
194
264
lambda-price-adjustment
lambda-price-adjustment
0.01
0.4
0.01
0.01
1
NIL
HORIZONTAL

TEXTBOX
19
514
231
546
Tax rates\n
26
0.0
1

SLIDER
16
271
195
304
minimum-liquidity-demand
minimum-liquidity-demand
0.01
0.2
0.02
0.01
1
NIL
HORIZONTAL

SLIDER
15
310
194
343
maximum-liquidity-demand
maximum-liquidity-demand
0.21
0.8
0.3
0.01
1
NIL
HORIZONTAL

MONITOR
1868
176
2095
221
Number of outstanding shares
sum [number-of-outstanding-shares] of NFCs
17
1
11

SLIDER
15
348
197
381
excess-demand-factor
excess-demand-factor
1000
15000
5000.0
1000
1
NIL
HORIZONTAL

TEXTBOX
1938
94
2105
116
Verification
16
0.0
1

MONITOR
1542
388
1812
433
Most successful strategy
most-successful-strategy
0
1
11

MONITOR
1543
440
1813
485
Investing strategy of most successful investor
[investing-strategy] of one-of most-successful-investor
0
1
11

MONITOR
1544
492
1813
537
Average learning score of fundamentalists
mean [fundamentalist-learning-score] of investors with [investing-strategy = \"fundamentalist\"]
17
1
11

MONITOR
1544
544
1814
589
Average learning score of chartists
mean [chartist-learning-score] of investors with [investing-strategy = \"chartist\"]
17
1
11

PLOT
1075
636
1506
819
Dividend rate of NFCs
ticks
Dividend rate
0.0
10.0
0.0
0.3
true
false
"" ""
PENS
"rate0" 1.0 0 -16777216 true "" "plot [dividend-payout-rate] of NFC 0"
"rate1" 1.0 0 -3844592 true "" "plot [dividend-payout-rate] of NFC 1"

SLIDER
426
597
811
630
dividend-increase
dividend-increase
0.01
0.20
0.05
0.01
1
NIL
HORIZONTAL

TEXTBOX
427
513
706
563
Decision-making rules of NFCs
20
0.0
1

CHOOSER
426
546
815
591
decision-rules-NFCs
decision-rules-NFCs
"max-shareholder-value-and-max-net-income" "max-shareholder-value-and-constant-payout-and-investment" "constant-payout-and-investment-and-max-net-income" "only-max-shareholder-value" "only-constant-payout-and-investment" "only-max-net-income"
3

SLIDER
24
662
196
695
tax-profit
tax-profit
0.01
0.50
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
15
386
196
419
minimum-interest-rate
minimum-interest-rate
0
0.06
0.02
0.01
1
NIL
HORIZONTAL

SLIDER
14
422
197
455
maximum-interest-rate
maximum-interest-rate
0.05
0.10
0.07
0.01
1
NIL
HORIZONTAL

SLIDER
427
635
811
668
percentage-of-net-income-to-payouts
percentage-of-net-income-to-payouts
0
1
0.4
0.1
1
NIL
HORIZONTAL

SLIDER
427
674
808
707
percentage-of-net-income-to-investment
percentage-of-net-income-to-investment
0
1
0.6
0.1
1
NIL
HORIZONTAL

PLOT
1078
392
1510
626
Real investment rate of NFCs
Ticks
Real investment rate
0.0
10.0
0.0
0.8
true
false
"" ""
PENS
"rate0" 1.0 0 -16777216 true "" "plot [real-investment-rate] of NFC 0"
"rate1" 1.0 0 -3844592 true "" "plot [real-investment-rate] of NFC 1"

PLOT
1074
1041
1507
1228
Debt of NFCs
ticks
Debt (USD)
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"nfc0" 1.0 0 -16777216 true "" "plot [debt] of NFC 0"
"nfc1" 1.0 0 -3844592 true "" "plot [debt] of NFC 1"

TEXTBOX
1250
92
1438
115
Output of NFCs
14
0.0
1

TEXTBOX
1625
93
1813
112
Output of investors
14
0.0
1

MONITOR
1870
227
2099
272
Investors with negative spendable liquidity
count investors with [spendable-liquidity < 0 or liquid-wealth < 0]
17
1
11

MONITOR
1870
278
2100
323
NFCs with negative investment rates
count NFCs with [real-investment-rate < 0]
17
1
11

SLIDER
429
714
810
747
return-on-real-investment-NFCs
return-on-real-investment-NFCs
0.01
0.10
0.1
0.01
1
NIL
HORIZONTAL

PLOT
1076
837
1506
1026
Share repurchasing rate of NFCs
ticks
Share repurchasing rate
0.0
10.0
0.0
0.02
true
false
"" ""
PENS
"NFC 0" 1.0 0 -16777216 true "" "plot [share-repurchasing-rate] of NFC 0"
"NFC 1" 1.0 0 -3844592 true "" "plot [share-repurchasing-rate] of NFC 1"

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

building institution
false
0
Rectangle -7500403 true true 0 60 300 270
Rectangle -16777216 true false 130 196 168 256
Rectangle -16777216 false false 0 255 300 270
Polygon -7500403 true true 0 60 150 15 300 60
Polygon -16777216 false false 0 60 150 15 300 60
Circle -1 true false 135 26 30
Circle -16777216 false false 135 25 30
Rectangle -16777216 false false 0 60 300 75
Rectangle -16777216 false false 218 75 255 90
Rectangle -16777216 false false 218 240 255 255
Rectangle -16777216 false false 224 90 249 240
Rectangle -16777216 false false 45 75 82 90
Rectangle -16777216 false false 45 240 82 255
Rectangle -16777216 false false 51 90 76 240
Rectangle -16777216 false false 90 240 127 255
Rectangle -16777216 false false 90 75 127 90
Rectangle -16777216 false false 96 90 121 240
Rectangle -16777216 false false 179 90 204 240
Rectangle -16777216 false false 173 75 210 90
Rectangle -16777216 false false 173 240 210 255
Rectangle -16777216 false false 269 90 294 240
Rectangle -16777216 false false 263 75 300 90
Rectangle -16777216 false false 263 240 300 255
Rectangle -16777216 false false 0 240 37 255
Rectangle -16777216 false false 6 90 31 240
Rectangle -16777216 false false 0 75 37 90
Line -16777216 false 112 260 184 260
Line -16777216 false 105 265 196 265

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

person business
false
0
Rectangle -1 true false 120 90 180 180
Polygon -13345367 true false 135 90 150 105 135 180 150 195 165 180 150 105 165 90
Polygon -7500403 true true 120 90 105 90 60 195 90 210 116 154 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 183 153 210 210 240 195 195 90 180 90 150 165
Circle -7500403 true true 110 5 80
Rectangle -7500403 true true 127 76 172 91
Line -16777216 false 172 90 161 94
Line -16777216 false 128 90 139 94
Polygon -13345367 true false 195 225 195 300 270 270 270 195
Rectangle -13791810 true false 180 225 195 300
Polygon -14835848 true false 180 226 195 226 270 196 255 196
Polygon -13345367 true false 209 202 209 216 244 202 243 188
Line -16777216 false 180 90 150 165
Line -16777216 false 120 90 150 165

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="test-scenario-analysis" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="120"/>
    <metric>[real-investment-rate] of NFC 0</metric>
    <metric>[real-investment-rate] of NFC 1</metric>
    <enumeratedValueSet variable="tax-dividend">
      <value value="0.1"/>
      <value value="0.3"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tax-capital-gain">
      <value value="0.1"/>
      <value value="0.3"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tax-profit">
      <value value="0.1"/>
      <value value="0.3"/>
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
