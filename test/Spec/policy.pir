(program
  (let
    (nonrec)
    (termbind
      (strict) (vardecl reconstructCaseError (con string)) (con string "PT1")
    )
    (datatypebind
      (datatype
        (tyvardecl Tuple2 (fun (type) (fun (type) (type))))
        (tyvardecl a (type)) (tyvardecl b (type))
        Tuple2_match
        (vardecl Tuple2 (fun a (fun b [ [ Tuple2 a ] b ])))
      )
    )
    (datatypebind
      (datatype (tyvardecl Unit (type))  Unit_match (vardecl Unit Unit))
    )
    (termbind (strict) (vardecl unitval (con unit)) (con unit ()))
    (termbind
      (strict)
      (vardecl
        fUnsafeFromDataTuple2_cunsafeFromBuiltinData
        (all
          a
          (type)
          (all
            b
            (type)
            (fun
              [ (lam a (type) (fun (con data) a)) a ]
              (fun
                [ (lam a (type) (fun (con data) a)) b ]
                (fun (con data) [ [ Tuple2 a ] b ])
              )
            )
          )
        )
      )
      (abs
        a
        (type)
        (abs
          b
          (type)
          (lam
            dUnsafeFromData
            [ (lam a (type) (fun (con data) a)) a ]
            (lam
              dUnsafeFromData
              [ (lam a (type) (fun (con data) a)) b ]
              (lam
                d
                (con data)
                (let
                  (nonrec)
                  (termbind
                    (nonstrict)
                    (vardecl
                      tup
                      [ [ (con pair) (con integer) ] [ (con list) (con data) ] ]
                    )
                    [ (builtin unConstrData) d ]
                  )
                  (termbind
                    (nonstrict)
                    (vardecl t [ (con list) (con data) ])
                    [
                      {
                        { (builtin sndPair) (con integer) }
                        [ (con list) (con data) ]
                      }
                      tup
                    ]
                  )
                  [
                    [
                      [
                        [
                          {
                            (builtin ifThenElse)
                            (fun (con unit) [ [ Tuple2 a ] b ])
                          }
                          [
                            [
                              (builtin equalsInteger)
                              [
                                {
                                  { (builtin fstPair) (con integer) }
                                  [ (con list) (con data) ]
                                }
                                tup
                              ]
                            ]
                            (con integer 0)
                          ]
                        ]
                        (lam
                          ds
                          (con unit)
                          [
                            [
                              { { Tuple2 a } b }
                              [
                                dUnsafeFromData
                                [ { (builtin headList) (con data) } t ]
                              ]
                            ]
                            [
                              dUnsafeFromData
                              [
                                { (builtin headList) (con data) }
                                [ { (builtin tailList) (con data) } t ]
                              ]
                            ]
                          ]
                        )
                      ]
                      (lam
                        ds
                        (con unit)
                        (let
                          (nonrec)
                          (termbind
                            (strict)
                            (vardecl thunk (con unit))
                            [
                              {
                                [
                                  Unit_match
                                  [
                                    [
                                      { (builtin trace) Unit }
                                      reconstructCaseError
                                    ]
                                    Unit
                                  ]
                                ]
                                (con unit)
                              }
                              unitval
                            ]
                          )
                          (error [ [ Tuple2 a ] b ])
                        )
                      )
                    ]
                    unitval
                  ]
                )
              )
            )
          )
        )
      )
    )
    (datatypebind
      (datatype
        (tyvardecl Credential (type))

        Credential_match
        (vardecl PubKeyCredential (fun (con bytestring) Credential))
        (vardecl ScriptCredential (fun (con bytestring) Credential))
      )
    )
    (termbind
      (strict)
      (vardecl
        fUnsafeFromDataCredential_cunsafeFromBuiltinData
        (fun (con data) Credential)
      )
      (lam
        d
        (con data)
        (let
          (nonrec)
          (termbind
            (nonstrict)
            (vardecl
              tup [ [ (con pair) (con integer) ] [ (con list) (con data) ] ]
            )
            [ (builtin unConstrData) d ]
          )
          (termbind
            (nonstrict)
            (vardecl index (con integer))
            [
              { { (builtin fstPair) (con integer) } [ (con list) (con data) ] }
              tup
            ]
          )
          [
            [
              [
                [
                  { (builtin ifThenElse) (fun (con unit) Credential) }
                  [ [ (builtin equalsInteger) index ] (con integer 1) ]
                ]
                (lam
                  ds
                  (con unit)
                  [
                    ScriptCredential
                    [
                      (builtin unBData)
                      [
                        { (builtin headList) (con data) }
                        [
                          {
                            { (builtin sndPair) (con integer) }
                            [ (con list) (con data) ]
                          }
                          tup
                        ]
                      ]
                    ]
                  ]
                )
              ]
              (lam
                ds
                (con unit)
                [
                  [
                    [
                      [
                        { (builtin ifThenElse) (fun (con unit) Credential) }
                        [ [ (builtin equalsInteger) index ] (con integer 0) ]
                      ]
                      (lam
                        ds
                        (con unit)
                        [
                          PubKeyCredential
                          [
                            (builtin unBData)
                            [
                              { (builtin headList) (con data) }
                              [
                                {
                                  { (builtin sndPair) (con integer) }
                                  [ (con list) (con data) ]
                                }
                                tup
                              ]
                            ]
                          ]
                        ]
                      )
                    ]
                    (lam
                      ds
                      (con unit)
                      (let
                        (nonrec)
                        (termbind
                          (strict)
                          (vardecl thunk (con unit))
                          [
                            {
                              [
                                Unit_match
                                [
                                  [
                                    { (builtin trace) Unit }
                                    reconstructCaseError
                                  ]
                                  Unit
                                ]
                              ]
                              (con unit)
                            }
                            unitval
                          ]
                        )
                        (error Credential)
                      )
                    )
                  ]
                  unitval
                ]
              )
            ]
            unitval
          ]
        )
      )
    )
    (datatypebind
      (datatype
        (tyvardecl StakingCredential (type))

        StakingCredential_match
        (vardecl StakingHash (fun Credential StakingCredential))
        (vardecl
          StakingPtr
          (fun
            (con integer)
            (fun (con integer) (fun (con integer) StakingCredential))
          )
        )
      )
    )
    (termbind
      (strict)
      (vardecl
        fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData
        (fun (con data) StakingCredential)
      )
      (lam
        d
        (con data)
        (let
          (nonrec)
          (termbind
            (nonstrict)
            (vardecl
              tup [ [ (con pair) (con integer) ] [ (con list) (con data) ] ]
            )
            [ (builtin unConstrData) d ]
          )
          (termbind
            (nonstrict)
            (vardecl t [ (con list) (con data) ])
            [
              { { (builtin sndPair) (con integer) } [ (con list) (con data) ] }
              tup
            ]
          )
          (termbind
            (nonstrict)
            (vardecl t [ (con list) (con data) ])
            [ { (builtin tailList) (con data) } t ]
          )
          (termbind
            (nonstrict)
            (vardecl index (con integer))
            [
              { { (builtin fstPair) (con integer) } [ (con list) (con data) ] }
              tup
            ]
          )
          [
            [
              [
                [
                  { (builtin ifThenElse) (fun (con unit) StakingCredential) }
                  [ [ (builtin equalsInteger) index ] (con integer 1) ]
                ]
                (lam
                  ds
                  (con unit)
                  [
                    [
                      [
                        StakingPtr
                        [
                          (builtin unIData)
                          [ { (builtin headList) (con data) } t ]
                        ]
                      ]
                      [
                        (builtin unIData)
                        [ { (builtin headList) (con data) } t ]
                      ]
                    ]
                    [
                      (builtin unIData)
                      [
                        { (builtin headList) (con data) }
                        [ { (builtin tailList) (con data) } t ]
                      ]
                    ]
                  ]
                )
              ]
              (lam
                ds
                (con unit)
                [
                  [
                    [
                      [
                        {
                          (builtin ifThenElse)
                          (fun (con unit) StakingCredential)
                        }
                        [ [ (builtin equalsInteger) index ] (con integer 0) ]
                      ]
                      (lam
                        ds
                        (con unit)
                        [
                          StakingHash
                          [
                            fUnsafeFromDataCredential_cunsafeFromBuiltinData
                            [
                              { (builtin headList) (con data) }
                              [
                                {
                                  { (builtin sndPair) (con integer) }
                                  [ (con list) (con data) ]
                                }
                                tup
                              ]
                            ]
                          ]
                        ]
                      )
                    ]
                    (lam
                      ds
                      (con unit)
                      (let
                        (nonrec)
                        (termbind
                          (strict)
                          (vardecl thunk (con unit))
                          [
                            {
                              [
                                Unit_match
                                [
                                  [
                                    { (builtin trace) Unit }
                                    reconstructCaseError
                                  ]
                                  Unit
                                ]
                              ]
                              (con unit)
                            }
                            unitval
                          ]
                        )
                        (error StakingCredential)
                      )
                    )
                  ]
                  unitval
                ]
              )
            ]
            unitval
          ]
        )
      )
    )
    (datatypebind
      (datatype
        (tyvardecl DCert (type))

        DCert_match
        (vardecl DCertDelegDeRegKey (fun StakingCredential DCert))
        (vardecl
          DCertDelegDelegate
          (fun StakingCredential (fun (con bytestring) DCert))
        )
        (vardecl DCertDelegRegKey (fun StakingCredential DCert))
        (vardecl DCertGenesis DCert)
        (vardecl DCertMir DCert)
        (vardecl
          DCertPoolRegister (fun (con bytestring) (fun (con bytestring) DCert))
        )
        (vardecl
          DCertPoolRetire (fun (con bytestring) (fun (con integer) DCert))
        )
      )
    )
    (termbind
      (strict)
      (vardecl
        fUnsafeFromDataDCert_cunsafeFromBuiltinData (fun (con data) DCert)
      )
      (lam
        d
        (con data)
        (let
          (nonrec)
          (termbind
            (nonstrict)
            (vardecl
              tup [ [ (con pair) (con integer) ] [ (con list) (con data) ] ]
            )
            [ (builtin unConstrData) d ]
          )
          (termbind
            (nonstrict)
            (vardecl t [ (con list) (con data) ])
            [
              { { (builtin sndPair) (con integer) } [ (con list) (con data) ] }
              tup
            ]
          )
          (termbind
            (nonstrict)
            (vardecl t [ (con list) (con data) ])
            [
              { { (builtin sndPair) (con integer) } [ (con list) (con data) ] }
              tup
            ]
          )
          (termbind
            (nonstrict)
            (vardecl t [ (con list) (con data) ])
            [
              { { (builtin sndPair) (con integer) } [ (con list) (con data) ] }
              tup
            ]
          )
          (termbind
            (nonstrict)
            (vardecl index (con integer))
            [
              { { (builtin fstPair) (con integer) } [ (con list) (con data) ] }
              tup
            ]
          )
          [
            [
              [
                [
                  { (builtin ifThenElse) (fun (con unit) DCert) }
                  [ [ (builtin equalsInteger) index ] (con integer 6) ]
                ]
                (lam ds (con unit) DCertMir)
              ]
              (lam
                ds
                (con unit)
                [
                  [
                    [
                      [
                        { (builtin ifThenElse) (fun (con unit) DCert) }
                        [ [ (builtin equalsInteger) index ] (con integer 5) ]
                      ]
                      (lam ds (con unit) DCertGenesis)
                    ]
                    (lam
                      ds
                      (con unit)
                      [
                        [
                          [
                            [
                              { (builtin ifThenElse) (fun (con unit) DCert) }
                              [
                                [ (builtin equalsInteger) index ]
                                (con integer 4)
                              ]
                            ]
                            (lam
                              ds
                              (con unit)
                              [
                                [
                                  DCertPoolRetire
                                  [
                                    (builtin unBData)
                                    [ { (builtin headList) (con data) } t ]
                                  ]
                                ]
                                [
                                  (builtin unIData)
                                  [
                                    { (builtin headList) (con data) }
                                    [ { (builtin tailList) (con data) } t ]
                                  ]
                                ]
                              ]
                            )
                          ]
                          (lam
                            ds
                            (con unit)
                            [
                              [
                                [
                                  [
                                    {
                                      (builtin ifThenElse)
                                      (fun (con unit) DCert)
                                    }
                                    [
                                      [ (builtin equalsInteger) index ]
                                      (con integer 3)
                                    ]
                                  ]
                                  (lam
                                    ds
                                    (con unit)
                                    [
                                      [
                                        DCertPoolRegister
                                        [
                                          (builtin unBData)
                                          [
                                            { (builtin headList) (con data) } t
                                          ]
                                        ]
                                      ]
                                      [
                                        (builtin unBData)
                                        [
                                          { (builtin headList) (con data) }
                                          [
                                            { (builtin tailList) (con data) } t
                                          ]
                                        ]
                                      ]
                                    ]
                                  )
                                ]
                                (lam
                                  ds
                                  (con unit)
                                  [
                                    [
                                      [
                                        [
                                          {
                                            (builtin ifThenElse)
                                            (fun (con unit) DCert)
                                          }
                                          [
                                            [ (builtin equalsInteger) index ]
                                            (con integer 2)
                                          ]
                                        ]
                                        (lam
                                          ds
                                          (con unit)
                                          [
                                            [
                                              DCertDelegDelegate
                                              [
                                                fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData
                                                [
                                                  {
                                                    (builtin headList)
                                                    (con data)
                                                  }
                                                  t
                                                ]
                                              ]
                                            ]
                                            [
                                              (builtin unBData)
                                              [
                                                {
                                                  (builtin headList) (con data)
                                                }
                                                [
                                                  {
                                                    (builtin tailList)
                                                    (con data)
                                                  }
                                                  t
                                                ]
                                              ]
                                            ]
                                          ]
                                        )
                                      ]
                                      (lam
                                        ds
                                        (con unit)
                                        [
                                          [
                                            [
                                              [
                                                {
                                                  (builtin ifThenElse)
                                                  (fun (con unit) DCert)
                                                }
                                                [
                                                  [
                                                    (builtin equalsInteger)
                                                    index
                                                  ]
                                                  (con integer 1)
                                                ]
                                              ]
                                              (lam
                                                ds
                                                (con unit)
                                                [
                                                  DCertDelegDeRegKey
                                                  [
                                                    fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData
                                                    [
                                                      {
                                                        (builtin headList)
                                                        (con data)
                                                      }
                                                      [
                                                        {
                                                          {
                                                            (builtin sndPair)
                                                            (con integer)
                                                          }
                                                          [
                                                            (con list)
                                                            (con data)
                                                          ]
                                                        }
                                                        tup
                                                      ]
                                                    ]
                                                  ]
                                                ]
                                              )
                                            ]
                                            (lam
                                              ds
                                              (con unit)
                                              [
                                                [
                                                  [
                                                    [
                                                      {
                                                        (builtin ifThenElse)
                                                        (fun (con unit) DCert)
                                                      }
                                                      [
                                                        [
                                                          (builtin
                                                            equalsInteger
                                                          )
                                                          index
                                                        ]
                                                        (con integer 0)
                                                      ]
                                                    ]
                                                    (lam
                                                      ds
                                                      (con unit)
                                                      [
                                                        DCertDelegRegKey
                                                        [
                                                          fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData
                                                          [
                                                            {
                                                              (builtin headList)
                                                              (con data)
                                                            }
                                                            [
                                                              {
                                                                {
                                                                  (builtin
                                                                    sndPair
                                                                  )
                                                                  (con integer)
                                                                }
                                                                [
                                                                  (con list)
                                                                  (con data)
                                                                ]
                                                              }
                                                              tup
                                                            ]
                                                          ]
                                                        ]
                                                      ]
                                                    )
                                                  ]
                                                  (lam
                                                    ds
                                                    (con unit)
                                                    (let
                                                      (nonrec)
                                                      (termbind
                                                        (strict)
                                                        (vardecl
                                                          thunk (con unit)
                                                        )
                                                        [
                                                          {
                                                            [
                                                              Unit_match
                                                              [
                                                                [
                                                                  {
                                                                    (builtin
                                                                      trace
                                                                    )
                                                                    Unit
                                                                  }
                                                                  reconstructCaseError
                                                                ]
                                                                Unit
                                                              ]
                                                            ]
                                                            (con unit)
                                                          }
                                                          unitval
                                                        ]
                                                      )
                                                      (error DCert)
                                                    )
                                                  )
                                                ]
                                                unitval
                                              ]
                                            )
                                          ]
                                          unitval
                                        ]
                                      )
                                    ]
                                    unitval
                                  ]
                                )
                              ]
                              unitval
                            ]
                          )
                        ]
                        unitval
                      ]
                    )
                  ]
                  unitval
                ]
              )
            ]
            unitval
          ]
        )
      )
    )
    (datatypebind
      (datatype
        (tyvardecl Bool (type))

        Bool_match
        (vardecl True Bool) (vardecl False Bool)
      )
    )
    (termbind
      (strict)
      (vardecl fUnsafeFromDataBool_cunsafeFromBuiltinData (fun (con data) Bool))
      (lam
        d
        (con data)
        (let
          (nonrec)
          (termbind
            (nonstrict)
            (vardecl index (con integer))
            [
              { { (builtin fstPair) (con integer) } [ (con list) (con data) ] }
              [ (builtin unConstrData) d ]
            ]
          )
          [
            [
              [
                [
                  { (builtin ifThenElse) (fun (con unit) Bool) }
                  [ [ (builtin equalsInteger) index ] (con integer 1) ]
                ]
                (lam ds (con unit) True)
              ]
              (lam
                ds
                (con unit)
                [
                  [
                    [
                      [
                        { (builtin ifThenElse) (fun (con unit) Bool) }
                        [ [ (builtin equalsInteger) index ] (con integer 0) ]
                      ]
                      (lam ds (con unit) False)
                    ]
                    (lam
                      ds
                      (con unit)
                      (let
                        (nonrec)
                        (termbind
                          (strict)
                          (vardecl thunk (con unit))
                          [
                            {
                              [
                                Unit_match
                                [
                                  [
                                    { (builtin trace) Unit }
                                    reconstructCaseError
                                  ]
                                  Unit
                                ]
                              ]
                              (con unit)
                            }
                            unitval
                          ]
                        )
                        (error Bool)
                      )
                    )
                  ]
                  unitval
                ]
              )
            ]
            unitval
          ]
        )
      )
    )
    (datatypebind
      (datatype
        (tyvardecl Extended (fun (type) (type)))
        (tyvardecl a (type))
        Extended_match
        (vardecl Finite (fun a [ Extended a ]))
        (vardecl NegInf [ Extended a ])
        (vardecl PosInf [ Extended a ])
      )
    )
    (termbind
      (strict)
      (vardecl
        fUnsafeFromDataExtended_cunsafeFromBuiltinData
        (all
          a
          (type)
          (fun
            [ (lam a (type) (fun (con data) a)) a ]
            (fun (con data) [ Extended a ])
          )
        )
      )
      (abs
        a
        (type)
        (lam
          dUnsafeFromData
          [ (lam a (type) (fun (con data) a)) a ]
          (lam
            d
            (con data)
            (let
              (nonrec)
              (termbind
                (nonstrict)
                (vardecl
                  tup [ [ (con pair) (con integer) ] [ (con list) (con data) ] ]
                )
                [ (builtin unConstrData) d ]
              )
              (termbind
                (nonstrict)
                (vardecl index (con integer))
                [
                  {
                    { (builtin fstPair) (con integer) }
                    [ (con list) (con data) ]
                  }
                  tup
                ]
              )
              [
                [
                  [
                    [
                      { (builtin ifThenElse) (fun (con unit) [ Extended a ]) }
                      [ [ (builtin equalsInteger) index ] (con integer 2) ]
                    ]
                    (lam ds (con unit) { PosInf a })
                  ]
                  (lam
                    ds
                    (con unit)
                    [
                      [
                        [
                          [
                            {
                              (builtin ifThenElse)
                              (fun (con unit) [ Extended a ])
                            }
                            [
                              [ (builtin equalsInteger) index ] (con integer 1)
                            ]
                          ]
                          (lam
                            ds
                            (con unit)
                            [
                              { Finite a }
                              [
                                dUnsafeFromData
                                [
                                  { (builtin headList) (con data) }
                                  [
                                    {
                                      { (builtin sndPair) (con integer) }
                                      [ (con list) (con data) ]
                                    }
                                    tup
                                  ]
                                ]
                              ]
                            ]
                          )
                        ]
                        (lam
                          ds
                          (con unit)
                          [
                            [
                              [
                                [
                                  {
                                    (builtin ifThenElse)
                                    (fun (con unit) [ Extended a ])
                                  }
                                  [
                                    [ (builtin equalsInteger) index ]
                                    (con integer 0)
                                  ]
                                ]
                                (lam ds (con unit) { NegInf a })
                              ]
                              (lam
                                ds
                                (con unit)
                                (let
                                  (nonrec)
                                  (termbind
                                    (strict)
                                    (vardecl thunk (con unit))
                                    [
                                      {
                                        [
                                          Unit_match
                                          [
                                            [
                                              { (builtin trace) Unit }
                                              reconstructCaseError
                                            ]
                                            Unit
                                          ]
                                        ]
                                        (con unit)
                                      }
                                      unitval
                                    ]
                                  )
                                  (error [ Extended a ])
                                )
                              )
                            ]
                            unitval
                          ]
                        )
                      ]
                      unitval
                    ]
                  )
                ]
                unitval
              ]
            )
          )
        )
      )
    )
    (termbind
      (strict)
      (vardecl
        fUnsafeFromDataTxId_cunsafeFromBuiltinData
        (fun (con data) (con bytestring))
      )
      (lam
        d
        (con data)
        (let
          (nonrec)
          (termbind
            (nonstrict)
            (vardecl
              tup [ [ (con pair) (con integer) ] [ (con list) (con data) ] ]
            )
            [ (builtin unConstrData) d ]
          )
          [
            [
              [
                [
                  { (builtin ifThenElse) (fun (con unit) (con bytestring)) }
                  [
                    [
                      (builtin equalsInteger)
                      [
                        {
                          { (builtin fstPair) (con integer) }
                          [ (con list) (con data) ]
                        }
                        tup
                      ]
                    ]
                    (con integer 0)
                  ]
                ]
                (lam
                  ds
                  (con unit)
                  [
                    (builtin unBData)
                    [
                      { (builtin headList) (con data) }
                      [
                        {
                          { (builtin sndPair) (con integer) }
                          [ (con list) (con data) ]
                        }
                        tup
                      ]
                    ]
                  ]
                )
              ]
              (lam
                ds
                (con unit)
                (let
                  (nonrec)
                  (termbind
                    (strict)
                    (vardecl thunk (con unit))
                    [
                      {
                        [
                          Unit_match
                          [
                            [ { (builtin trace) Unit } reconstructCaseError ]
                            Unit
                          ]
                        ]
                        (con unit)
                      }
                      unitval
                    ]
                  )
                  (error (con bytestring))
                )
              )
            ]
            unitval
          ]
        )
      )
    )
    (datatypebind
      (datatype
        (tyvardecl TxOutRef (type))

        TxOutRef_match
        (vardecl TxOutRef (fun (con bytestring) (fun (con integer) TxOutRef)))
      )
    )
    (termbind
      (strict)
      (vardecl
        fUnsafeFromDataTxOutRef_cunsafeFromBuiltinData (fun (con data) TxOutRef)
      )
      (lam
        d
        (con data)
        (let
          (nonrec)
          (termbind
            (nonstrict)
            (vardecl
              tup [ [ (con pair) (con integer) ] [ (con list) (con data) ] ]
            )
            [ (builtin unConstrData) d ]
          )
          (termbind
            (nonstrict)
            (vardecl t [ (con list) (con data) ])
            [
              { { (builtin sndPair) (con integer) } [ (con list) (con data) ] }
              tup
            ]
          )
          [
            [
              [
                [
                  { (builtin ifThenElse) (fun (con unit) TxOutRef) }
                  [
                    [
                      (builtin equalsInteger)
                      [
                        {
                          { (builtin fstPair) (con integer) }
                          [ (con list) (con data) ]
                        }
                        tup
                      ]
                    ]
                    (con integer 0)
                  ]
                ]
                (lam
                  ds
                  (con unit)
                  [
                    [
                      TxOutRef
                      [
                        fUnsafeFromDataTxId_cunsafeFromBuiltinData
                        [ { (builtin headList) (con data) } t ]
                      ]
                    ]
                    [
                      (builtin unIData)
                      [
                        { (builtin headList) (con data) }
                        [ { (builtin tailList) (con data) } t ]
                      ]
                    ]
                  ]
                )
              ]
              (lam
                ds
                (con unit)
                (let
                  (nonrec)
                  (termbind
                    (strict)
                    (vardecl thunk (con unit))
                    [
                      {
                        [
                          Unit_match
                          [
                            [ { (builtin trace) Unit } reconstructCaseError ]
                            Unit
                          ]
                        ]
                        (con unit)
                      }
                      unitval
                    ]
                  )
                  (error TxOutRef)
                )
              )
            ]
            unitval
          ]
        )
      )
    )
    (datatypebind
      (datatype
        (tyvardecl Maybe (fun (type) (type)))
        (tyvardecl a (type))
        Maybe_match
        (vardecl Just (fun a [ Maybe a ])) (vardecl Nothing [ Maybe a ])
      )
    )
    (termbind
      (strict)
      (vardecl
        fUnsafeFromDataMaybe_cunsafeFromBuiltinData
        (all
          a
          (type)
          (fun
            [ (lam a (type) (fun (con data) a)) a ] (fun (con data) [ Maybe a ])
          )
        )
      )
      (abs
        a
        (type)
        (lam
          dUnsafeFromData
          [ (lam a (type) (fun (con data) a)) a ]
          (lam
            d
            (con data)
            (let
              (nonrec)
              (termbind
                (nonstrict)
                (vardecl
                  tup [ [ (con pair) (con integer) ] [ (con list) (con data) ] ]
                )
                [ (builtin unConstrData) d ]
              )
              (termbind
                (nonstrict)
                (vardecl index (con integer))
                [
                  {
                    { (builtin fstPair) (con integer) }
                    [ (con list) (con data) ]
                  }
                  tup
                ]
              )
              [
                [
                  [
                    [
                      { (builtin ifThenElse) (fun (con unit) [ Maybe a ]) }
                      [ [ (builtin equalsInteger) index ] (con integer 0) ]
                    ]
                    (lam
                      ds
                      (con unit)
                      [
                        { Just a }
                        [
                          dUnsafeFromData
                          [
                            { (builtin headList) (con data) }
                            [
                              {
                                { (builtin sndPair) (con integer) }
                                [ (con list) (con data) ]
                              }
                              tup
                            ]
                          ]
                        ]
                      ]
                    )
                  ]
                  (lam
                    ds
                    (con unit)
                    [
                      [
                        [
                          [
                            {
                              (builtin ifThenElse) (fun (con unit) [ Maybe a ])
                            }
                            [
                              [ (builtin equalsInteger) index ] (con integer 1)
                            ]
                          ]
                          (lam ds (con unit) { Nothing a })
                        ]
                        (lam
                          ds
                          (con unit)
                          (let
                            (nonrec)
                            (termbind
                              (strict)
                              (vardecl thunk (con unit))
                              [
                                {
                                  [
                                    Unit_match
                                    [
                                      [
                                        { (builtin trace) Unit }
                                        reconstructCaseError
                                      ]
                                      Unit
                                    ]
                                  ]
                                  (con unit)
                                }
                                unitval
                              ]
                            )
                            (error [ Maybe a ])
                          )
                        )
                      ]
                      unitval
                    ]
                  )
                ]
                unitval
              ]
            )
          )
        )
      )
    )
    (let
      (rec)
      (datatypebind
        (datatype
          (tyvardecl List (fun (type) (type)))
          (tyvardecl a (type))
          Nil_match
          (vardecl Nil [ List a ])
          (vardecl Cons (fun a (fun [ List a ] [ List a ])))
        )
      )
      (let
        (nonrec)
        (termbind
          (strict)
          (vardecl
            fUnsafeFromDataMap_cunsafeFromBuiltinData
            (all
              k
              (type)
              (all
                v
                (type)
                (fun
                  [ (lam a (type) (fun (con data) a)) k ]
                  (fun
                    [ (lam a (type) (fun (con data) a)) v ]
                    (fun
                      (con data)
                      [
                        [
                          (lam
                            k (type) (lam v (type) [ List [ [ Tuple2 k ] v ] ])
                          )
                          k
                        ]
                        v
                      ]
                    )
                  )
                )
              )
            )
          )
          (abs
            k
            (type)
            (abs
              v
              (type)
              (lam
                dUnsafeFromData
                [ (lam a (type) (fun (con data) a)) k ]
                (lam
                  dUnsafeFromData
                  [ (lam a (type) (fun (con data) a)) v ]
                  (let
                    (rec)
                    (termbind
                      (strict)
                      (vardecl
                        go
                        (fun
                          [
                            (con list) [ [ (con pair) (con data) ] (con data) ]
                          ]
                          [ List [ [ Tuple2 k ] v ] ]
                        )
                      )
                      (lam
                        l
                        [ (con list) [ [ (con pair) (con data) ] (con data) ] ]
                        (let
                          (nonrec)
                          (termbind
                            (nonstrict)
                            (vardecl
                              tup [ [ (con pair) (con data) ] (con data) ]
                            )
                            [
                              {
                                (builtin headList)
                                [ [ (con pair) (con data) ] (con data) ]
                              }
                              l
                            ]
                          )
                          [
                            [
                              [
                                [
                                  {
                                    {
                                      (builtin chooseList)
                                      [ [ (con pair) (con data) ] (con data) ]
                                    }
                                    (fun Unit [ List [ [ Tuple2 k ] v ] ])
                                  }
                                  l
                                ]
                                (lam ds Unit { Nil [ [ Tuple2 k ] v ] })
                              ]
                              (lam
                                ds
                                Unit
                                [
                                  [
                                    { Cons [ [ Tuple2 k ] v ] }
                                    [
                                      [
                                        { { Tuple2 k } v }
                                        [
                                          dUnsafeFromData
                                          [
                                            {
                                              { (builtin fstPair) (con data) }
                                              (con data)
                                            }
                                            tup
                                          ]
                                        ]
                                      ]
                                      [
                                        dUnsafeFromData
                                        [
                                          {
                                            { (builtin sndPair) (con data) }
                                            (con data)
                                          }
                                          tup
                                        ]
                                      ]
                                    ]
                                  ]
                                  [
                                    go
                                    [
                                      {
                                        (builtin tailList)
                                        [ [ (con pair) (con data) ] (con data) ]
                                      }
                                      l
                                    ]
                                  ]
                                ]
                              )
                            ]
                            Unit
                          ]
                        )
                      )
                    )
                    (lam d (con data) [ go [ (builtin unMapData) d ] ])
                  )
                )
              )
            )
          )
        )
        (termbind
          (nonstrict)
          (vardecl
            fUnsafeFromDataValue
            (fun
              (con data)
              [
                [
                  (lam k (type) (lam v (type) [ List [ [ Tuple2 k ] v ] ]))
                  (con bytestring)
                ]
                (con integer)
              ]
            )
          )
          [
            [
              {
                { fUnsafeFromDataMap_cunsafeFromBuiltinData (con bytestring) }
                (con integer)
              }
              (builtin unBData)
            ]
            (builtin unIData)
          ]
        )
        (datatypebind
          (datatype
            (tyvardecl Address (type))

            Address_match
            (vardecl
              Address (fun Credential (fun [ Maybe StakingCredential ] Address))
            )
          )
        )
        (datatypebind
          (datatype
            (tyvardecl TxOut (type))

            TxOut_match
            (vardecl
              TxOut
              (fun
                Address
                (fun
                  [
                    [
                      (lam k (type) (lam v (type) [ List [ [ Tuple2 k ] v ] ]))
                      (con bytestring)
                    ]
                    [
                      [
                        (lam
                          k (type) (lam v (type) [ List [ [ Tuple2 k ] v ] ])
                        )
                        (con bytestring)
                      ]
                      (con integer)
                    ]
                  ]
                  (fun [ Maybe (con bytestring) ] TxOut)
                )
              )
            )
          )
        )
        (termbind
          (strict)
          (vardecl
            fUnsafeFromDataTxOut_cunsafeFromBuiltinData (fun (con data) TxOut)
          )
          (lam
            d
            (con data)
            (let
              (nonrec)
              (termbind
                (nonstrict)
                (vardecl
                  tup [ [ (con pair) (con integer) ] [ (con list) (con data) ] ]
                )
                [ (builtin unConstrData) d ]
              )
              (termbind
                (nonstrict)
                (vardecl t [ (con list) (con data) ])
                [
                  {
                    { (builtin sndPair) (con integer) }
                    [ (con list) (con data) ]
                  }
                  tup
                ]
              )
              (termbind
                (nonstrict)
                (vardecl t [ (con list) (con data) ])
                [ { (builtin tailList) (con data) } t ]
              )
              [
                [
                  [
                    [
                      { (builtin ifThenElse) (fun (con unit) TxOut) }
                      [
                        [
                          (builtin equalsInteger)
                          [
                            {
                              { (builtin fstPair) (con integer) }
                              [ (con list) (con data) ]
                            }
                            tup
                          ]
                        ]
                        (con integer 0)
                      ]
                    ]
                    (lam
                      ds
                      (con unit)
                      [
                        [
                          [
                            TxOut
                            (let
                              (nonrec)
                              (termbind
                                (strict)
                                (vardecl d (con data))
                                [ { (builtin headList) (con data) } t ]
                              )
                              (termbind
                                (nonstrict)
                                (vardecl
                                  tup
                                  [
                                    [ (con pair) (con integer) ]
                                    [ (con list) (con data) ]
                                  ]
                                )
                                [ (builtin unConstrData) d ]
                              )
                              (termbind
                                (nonstrict)
                                (vardecl t [ (con list) (con data) ])
                                [
                                  {
                                    { (builtin sndPair) (con integer) }
                                    [ (con list) (con data) ]
                                  }
                                  tup
                                ]
                              )
                              [
                                [
                                  [
                                    [
                                      {
                                        (builtin ifThenElse)
                                        (fun (con unit) Address)
                                      }
                                      [
                                        [
                                          (builtin equalsInteger)
                                          [
                                            {
                                              {
                                                (builtin fstPair) (con integer)
                                              }
                                              [ (con list) (con data) ]
                                            }
                                            tup
                                          ]
                                        ]
                                        (con integer 0)
                                      ]
                                    ]
                                    (lam
                                      ds
                                      (con unit)
                                      [
                                        [
                                          Address
                                          [
                                            fUnsafeFromDataCredential_cunsafeFromBuiltinData
                                            [
                                              { (builtin headList) (con data) }
                                              t
                                            ]
                                          ]
                                        ]
                                        [
                                          [
                                            {
                                              fUnsafeFromDataMaybe_cunsafeFromBuiltinData
                                              StakingCredential
                                            }
                                            fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData
                                          ]
                                          [
                                            { (builtin headList) (con data) }
                                            [
                                              { (builtin tailList) (con data) }
                                              t
                                            ]
                                          ]
                                        ]
                                      ]
                                    )
                                  ]
                                  (lam
                                    ds
                                    (con unit)
                                    (let
                                      (nonrec)
                                      (termbind
                                        (strict)
                                        (vardecl thunk (con unit))
                                        [
                                          {
                                            [
                                              Unit_match
                                              [
                                                [
                                                  { (builtin trace) Unit }
                                                  reconstructCaseError
                                                ]
                                                Unit
                                              ]
                                            ]
                                            (con unit)
                                          }
                                          unitval
                                        ]
                                      )
                                      (error Address)
                                    )
                                  )
                                ]
                                unitval
                              ]
                            )
                          ]
                          [
                            [
                              [
                                {
                                  {
                                    fUnsafeFromDataMap_cunsafeFromBuiltinData
                                    (con bytestring)
                                  }
                                  [
                                    [
                                      (lam
                                        k
                                        (type)
                                        (lam
                                          v (type) [ List [ [ Tuple2 k ] v ] ]
                                        )
                                      )
                                      (con bytestring)
                                    ]
                                    (con integer)
                                  ]
                                }
                                (builtin unBData)
                              ]
                              fUnsafeFromDataValue
                            ]
                            [ { (builtin headList) (con data) } t ]
                          ]
                        ]
                        [
                          [
                            {
                              fUnsafeFromDataMaybe_cunsafeFromBuiltinData
                              (con bytestring)
                            }
                            (builtin unBData)
                          ]
                          [
                            { (builtin headList) (con data) }
                            [ { (builtin tailList) (con data) } t ]
                          ]
                        ]
                      ]
                    )
                  ]
                  (lam
                    ds
                    (con unit)
                    (let
                      (nonrec)
                      (termbind
                        (strict)
                        (vardecl thunk (con unit))
                        [
                          {
                            [
                              Unit_match
                              [
                                [
                                  { (builtin trace) Unit } reconstructCaseError
                                ]
                                Unit
                              ]
                            ]
                            (con unit)
                          }
                          unitval
                        ]
                      )
                      (error TxOut)
                    )
                  )
                ]
                unitval
              ]
            )
          )
        )
        (termbind
          (strict)
          (vardecl
            fUnsafeFromDataNil_cunsafeFromBuiltinData
            (all
              a
              (type)
              (fun
                [ (lam a (type) (fun (con data) a)) a ]
                (fun (con data) [ List a ])
              )
            )
          )
          (abs
            a
            (type)
            (lam
              dUnsafeFromData
              [ (lam a (type) (fun (con data) a)) a ]
              (let
                (rec)
                (termbind
                  (strict)
                  (vardecl go (fun [ (con list) (con data) ] [ List a ]))
                  (lam
                    l
                    [ (con list) (con data) ]
                    [
                      [
                        [
                          [
                            {
                              { (builtin chooseList) (con data) }
                              (fun Unit [ List a ])
                            }
                            l
                          ]
                          (lam ds Unit { Nil a })
                        ]
                        (lam
                          ds
                          Unit
                          [
                            [
                              { Cons a }
                              [
                                dUnsafeFromData
                                [ { (builtin headList) (con data) } l ]
                              ]
                            ]
                            [ go [ { (builtin tailList) (con data) } l ] ]
                          ]
                        )
                      ]
                      Unit
                    ]
                  )
                )
                (lam d (con data) [ go [ (builtin unListData) d ] ])
              )
            )
          )
        )
        (datatypebind
          (datatype
            (tyvardecl ScriptPurpose (type))

            ScriptPurpose_match
            (vardecl Certifying (fun DCert ScriptPurpose))
            (vardecl Minting (fun (con bytestring) ScriptPurpose))
            (vardecl Rewarding (fun StakingCredential ScriptPurpose))
            (vardecl Spending (fun TxOutRef ScriptPurpose))
          )
        )
        (datatypebind
          (datatype
            (tyvardecl LowerBound (fun (type) (type)))
            (tyvardecl a (type))
            LowerBound_match
            (vardecl
              LowerBound (fun [ Extended a ] (fun Bool [ LowerBound a ]))
            )
          )
        )
        (datatypebind
          (datatype
            (tyvardecl UpperBound (fun (type) (type)))
            (tyvardecl a (type))
            UpperBound_match
            (vardecl
              UpperBound (fun [ Extended a ] (fun Bool [ UpperBound a ]))
            )
          )
        )
        (datatypebind
          (datatype
            (tyvardecl Interval (fun (type) (type)))
            (tyvardecl a (type))
            Interval_match
            (vardecl
              Interval
              (fun [ LowerBound a ] (fun [ UpperBound a ] [ Interval a ]))
            )
          )
        )
        (datatypebind
          (datatype
            (tyvardecl TxInInfo (type))

            TxInInfo_match
            (vardecl TxInInfo (fun TxOutRef (fun TxOut TxInInfo)))
          )
        )
        (datatypebind
          (datatype
            (tyvardecl TxInfo (type))

            TxInfo_match
            (vardecl
              TxInfo
              (fun
                [ List TxInInfo ]
                (fun
                  [ List TxOut ]
                  (fun
                    [
                      [
                        (lam
                          k (type) (lam v (type) [ List [ [ Tuple2 k ] v ] ])
                        )
                        (con bytestring)
                      ]
                      [
                        [
                          (lam
                            k (type) (lam v (type) [ List [ [ Tuple2 k ] v ] ])
                          )
                          (con bytestring)
                        ]
                        (con integer)
                      ]
                    ]
                    (fun
                      [
                        [
                          (lam
                            k (type) (lam v (type) [ List [ [ Tuple2 k ] v ] ])
                          )
                          (con bytestring)
                        ]
                        [
                          [
                            (lam
                              k
                              (type)
                              (lam v (type) [ List [ [ Tuple2 k ] v ] ])
                            )
                            (con bytestring)
                          ]
                          (con integer)
                        ]
                      ]
                      (fun
                        [ List DCert ]
                        (fun
                          [
                            List [ [ Tuple2 StakingCredential ] (con integer) ]
                          ]
                          (fun
                            [ Interval (con integer) ]
                            (fun
                              [ List (con bytestring) ]
                              (fun
                                [
                                  List
                                  [ [ Tuple2 (con bytestring) ] (con data) ]
                                ]
                                (fun (con bytestring) TxInfo)
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
        (datatypebind
          (datatype
            (tyvardecl ScriptContext (type))

            ScriptContext_match
            (vardecl
              ScriptContext (fun TxInfo (fun ScriptPurpose ScriptContext))
            )
          )
        )
        (lam
          r
          (con data)
          (lam
            p
            (con data)
            (let
              (nonrec)
              (termbind
                (nonstrict)
                (vardecl
                  tup [ [ (con pair) (con integer) ] [ (con list) (con data) ] ]
                )
                [ (builtin unConstrData) p ]
              )
              (termbind
                (nonstrict)
                (vardecl t [ (con list) (con data) ])
                [
                  {
                    { (builtin sndPair) (con integer) }
                    [ (con list) (con data) ]
                  }
                  tup
                ]
              )
              {
                [
                  [
                    {
                      [
                        Bool_match
                        (let
                          (nonrec)
                          (termbind
                            (strict)
                            (vardecl ds Unit)
                            [
                              [
                                [
                                  [
                                    {
                                      (builtin ifThenElse) (fun (con unit) Unit)
                                    }
                                    [
                                      [
                                        (builtin equalsInteger)
                                        [
                                          {
                                            { (builtin fstPair) (con integer) }
                                            [ (con list) (con data) ]
                                          }
                                          [ (builtin unConstrData) r ]
                                        ]
                                      ]
                                      (con integer 0)
                                    ]
                                  ]
                                  (lam ds (con unit) Unit)
                                ]
                                (lam
                                  ds
                                  (con unit)
                                  (let
                                    (nonrec)
                                    (termbind
                                      (strict)
                                      (vardecl thunk (con unit))
                                      [
                                        {
                                          [
                                            Unit_match
                                            [
                                              [
                                                { (builtin trace) Unit }
                                                reconstructCaseError
                                              ]
                                              Unit
                                            ]
                                          ]
                                          (con unit)
                                        }
                                        unitval
                                      ]
                                    )
                                    (error Unit)
                                  )
                                )
                              ]
                              unitval
                            ]
                          )
                          (termbind
                            (strict)
                            (vardecl ds ScriptContext)
                            [
                              [
                                [
                                  [
                                    {
                                      (builtin ifThenElse)
                                      (fun (con unit) ScriptContext)
                                    }
                                    [
                                      [
                                        (builtin equalsInteger)
                                        [
                                          {
                                            { (builtin fstPair) (con integer) }
                                            [ (con list) (con data) ]
                                          }
                                          tup
                                        ]
                                      ]
                                      (con integer 0)
                                    ]
                                  ]
                                  (lam
                                    ds
                                    (con unit)
                                    [
                                      [
                                        ScriptContext
                                        (let
                                          (nonrec)
                                          (termbind
                                            (strict)
                                            (vardecl d (con data))
                                            [
                                              { (builtin headList) (con data) }
                                              t
                                            ]
                                          )
                                          (termbind
                                            (nonstrict)
                                            (vardecl
                                              tup
                                              [
                                                [ (con pair) (con integer) ]
                                                [ (con list) (con data) ]
                                              ]
                                            )
                                            [ (builtin unConstrData) d ]
                                          )
                                          (termbind
                                            (nonstrict)
                                            (vardecl
                                              t [ (con list) (con data) ]
                                            )
                                            [
                                              {
                                                {
                                                  (builtin sndPair)
                                                  (con integer)
                                                }
                                                [ (con list) (con data) ]
                                              }
                                              tup
                                            ]
                                          )
                                          (termbind
                                            (nonstrict)
                                            (vardecl
                                              t [ (con list) (con data) ]
                                            )
                                            [
                                              { (builtin tailList) (con data) }
                                              t
                                            ]
                                          )
                                          (termbind
                                            (nonstrict)
                                            (vardecl
                                              t [ (con list) (con data) ]
                                            )
                                            [
                                              { (builtin tailList) (con data) }
                                              t
                                            ]
                                          )
                                          (termbind
                                            (nonstrict)
                                            (vardecl
                                              t [ (con list) (con data) ]
                                            )
                                            [
                                              { (builtin tailList) (con data) }
                                              t
                                            ]
                                          )
                                          (termbind
                                            (nonstrict)
                                            (vardecl
                                              t [ (con list) (con data) ]
                                            )
                                            [
                                              { (builtin tailList) (con data) }
                                              t
                                            ]
                                          )
                                          (termbind
                                            (nonstrict)
                                            (vardecl
                                              t [ (con list) (con data) ]
                                            )
                                            [
                                              { (builtin tailList) (con data) }
                                              t
                                            ]
                                          )
                                          (termbind
                                            (nonstrict)
                                            (vardecl
                                              t [ (con list) (con data) ]
                                            )
                                            [
                                              { (builtin tailList) (con data) }
                                              t
                                            ]
                                          )
                                          (termbind
                                            (nonstrict)
                                            (vardecl
                                              t [ (con list) (con data) ]
                                            )
                                            [
                                              { (builtin tailList) (con data) }
                                              t
                                            ]
                                          )
                                          (termbind
                                            (nonstrict)
                                            (vardecl
                                              t [ (con list) (con data) ]
                                            )
                                            [
                                              { (builtin tailList) (con data) }
                                              t
                                            ]
                                          )
                                          [
                                            [
                                              [
                                                [
                                                  {
                                                    (builtin ifThenElse)
                                                    (fun (con unit) TxInfo)
                                                  }
                                                  [
                                                    [
                                                      (builtin equalsInteger)
                                                      [
                                                        {
                                                          {
                                                            (builtin fstPair)
                                                            (con integer)
                                                          }
                                                          [
                                                            (con list)
                                                            (con data)
                                                          ]
                                                        }
                                                        tup
                                                      ]
                                                    ]
                                                    (con integer 0)
                                                  ]
                                                ]
                                                (lam
                                                  ds
                                                  (con unit)
                                                  [
                                                    [
                                                      [
                                                        [
                                                          [
                                                            [
                                                              [
                                                                [
                                                                  [
                                                                    [
                                                                      TxInfo
                                                                      [
                                                                        [
                                                                          {
                                                                            fUnsafeFromDataNil_cunsafeFromBuiltinData
                                                                            TxInInfo
                                                                          }
                                                                          (lam
                                                                            d
                                                                            (con
                                                                              data
                                                                            )
                                                                            (let
                                                                              (nonrec)
                                                                              (termbind
                                                                                (nonstrict)
                                                                                (vardecl
                                                                                  tup
                                                                                  [
                                                                                    [
                                                                                      (con
                                                                                        pair
                                                                                      )
                                                                                      (con
                                                                                        integer
                                                                                      )
                                                                                    ]
                                                                                    [
                                                                                      (con
                                                                                        list
                                                                                      )
                                                                                      (con
                                                                                        data
                                                                                      )
                                                                                    ]
                                                                                  ]
                                                                                )
                                                                                [
                                                                                  (builtin
                                                                                    unConstrData
                                                                                  )
                                                                                  d
                                                                                ]
                                                                              )
                                                                              (termbind
                                                                                (nonstrict)
                                                                                (vardecl
                                                                                  t
                                                                                  [
                                                                                    (con
                                                                                      list
                                                                                    )
                                                                                    (con
                                                                                      data
                                                                                    )
                                                                                  ]
                                                                                )
                                                                                [
                                                                                  {
                                                                                    {
                                                                                      (builtin
                                                                                        sndPair
                                                                                      )
                                                                                      (con
                                                                                        integer
                                                                                      )
                                                                                    }
                                                                                    [
                                                                                      (con
                                                                                        list
                                                                                      )
                                                                                      (con
                                                                                        data
                                                                                      )
                                                                                    ]
                                                                                  }
                                                                                  tup
                                                                                ]
                                                                              )
                                                                              [
                                                                                [
                                                                                  [
                                                                                    [
                                                                                      {
                                                                                        (builtin
                                                                                          ifThenElse
                                                                                        )
                                                                                        (fun
                                                                                          (con
                                                                                            unit
                                                                                          )
                                                                                          TxInInfo
                                                                                        )
                                                                                      }
                                                                                      [
                                                                                        [
                                                                                          (builtin
                                                                                            equalsInteger
                                                                                          )
                                                                                          [
                                                                                            {
                                                                                              {
                                                                                                (builtin
                                                                                                  fstPair
                                                                                                )
                                                                                                (con
                                                                                                  integer
                                                                                                )
                                                                                              }
                                                                                              [
                                                                                                (con
                                                                                                  list
                                                                                                )
                                                                                                (con
                                                                                                  data
                                                                                                )
                                                                                              ]
                                                                                            }
                                                                                            tup
                                                                                          ]
                                                                                        ]
                                                                                        (con
                                                                                          integer
                                                                                          0
                                                                                        )
                                                                                      ]
                                                                                    ]
                                                                                    (lam
                                                                                      ds
                                                                                      (con
                                                                                        unit
                                                                                      )
                                                                                      [
                                                                                        [
                                                                                          TxInInfo
                                                                                          [
                                                                                            fUnsafeFromDataTxOutRef_cunsafeFromBuiltinData
                                                                                            [
                                                                                              {
                                                                                                (builtin
                                                                                                  headList
                                                                                                )
                                                                                                (con
                                                                                                  data
                                                                                                )
                                                                                              }
                                                                                              t
                                                                                            ]
                                                                                          ]
                                                                                        ]
                                                                                        [
                                                                                          fUnsafeFromDataTxOut_cunsafeFromBuiltinData
                                                                                          [
                                                                                            {
                                                                                              (builtin
                                                                                                headList
                                                                                              )
                                                                                              (con
                                                                                                data
                                                                                              )
                                                                                            }
                                                                                            [
                                                                                              {
                                                                                                (builtin
                                                                                                  tailList
                                                                                                )
                                                                                                (con
                                                                                                  data
                                                                                                )
                                                                                              }
                                                                                              t
                                                                                            ]
                                                                                          ]
                                                                                        ]
                                                                                      ]
                                                                                    )
                                                                                  ]
                                                                                  (lam
                                                                                    ds
                                                                                    (con
                                                                                      unit
                                                                                    )
                                                                                    (let
                                                                                      (nonrec)
                                                                                      (termbind
                                                                                        (strict)
                                                                                        (vardecl
                                                                                          thunk
                                                                                          (con
                                                                                            unit
                                                                                          )
                                                                                        )
                                                                                        [
                                                                                          {
                                                                                            [
                                                                                              Unit_match
                                                                                              [
                                                                                                [
                                                                                                  {
                                                                                                    (builtin
                                                                                                      trace
                                                                                                    )
                                                                                                    Unit
                                                                                                  }
                                                                                                  reconstructCaseError
                                                                                                ]
                                                                                                Unit
                                                                                              ]
                                                                                            ]
                                                                                            (con
                                                                                              unit
                                                                                            )
                                                                                          }
                                                                                          unitval
                                                                                        ]
                                                                                      )
                                                                                      (error
                                                                                        TxInInfo
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                ]
                                                                                unitval
                                                                              ]
                                                                            )
                                                                          )
                                                                        ]
                                                                        [
                                                                          {
                                                                            (builtin
                                                                              headList
                                                                            )
                                                                            (con
                                                                              data
                                                                            )
                                                                          }
                                                                          t
                                                                        ]
                                                                      ]
                                                                    ]
                                                                    [
                                                                      [
                                                                        {
                                                                          fUnsafeFromDataNil_cunsafeFromBuiltinData
                                                                          TxOut
                                                                        }
                                                                        fUnsafeFromDataTxOut_cunsafeFromBuiltinData
                                                                      ]
                                                                      [
                                                                        {
                                                                          (builtin
                                                                            headList
                                                                          )
                                                                          (con
                                                                            data
                                                                          )
                                                                        }
                                                                        t
                                                                      ]
                                                                    ]
                                                                  ]
                                                                  [
                                                                    [
                                                                      [
                                                                        {
                                                                          {
                                                                            fUnsafeFromDataMap_cunsafeFromBuiltinData
                                                                            (con
                                                                              bytestring
                                                                            )
                                                                          }
                                                                          [
                                                                            [
                                                                              (lam
                                                                                k
                                                                                (type)
                                                                                (lam
                                                                                  v
                                                                                  (type)
                                                                                  [
                                                                                    List
                                                                                    [
                                                                                      [
                                                                                        Tuple2
                                                                                        k
                                                                                      ]
                                                                                      v
                                                                                    ]
                                                                                  ]
                                                                                )
                                                                              )
                                                                              (con
                                                                                bytestring
                                                                              )
                                                                            ]
                                                                            (con
                                                                              integer
                                                                            )
                                                                          ]
                                                                        }
                                                                        (builtin
                                                                          unBData
                                                                        )
                                                                      ]
                                                                      fUnsafeFromDataValue
                                                                    ]
                                                                    [
                                                                      {
                                                                        (builtin
                                                                          headList
                                                                        )
                                                                        (con
                                                                          data
                                                                        )
                                                                      }
                                                                      t
                                                                    ]
                                                                  ]
                                                                ]
                                                                [
                                                                  [
                                                                    [
                                                                      {
                                                                        {
                                                                          fUnsafeFromDataMap_cunsafeFromBuiltinData
                                                                          (con
                                                                            bytestring
                                                                          )
                                                                        }
                                                                        [
                                                                          [
                                                                            (lam
                                                                              k
                                                                              (type)
                                                                              (lam
                                                                                v
                                                                                (type)
                                                                                [
                                                                                  List
                                                                                  [
                                                                                    [
                                                                                      Tuple2
                                                                                      k
                                                                                    ]
                                                                                    v
                                                                                  ]
                                                                                ]
                                                                              )
                                                                            )
                                                                            (con
                                                                              bytestring
                                                                            )
                                                                          ]
                                                                          (con
                                                                            integer
                                                                          )
                                                                        ]
                                                                      }
                                                                      (builtin
                                                                        unBData
                                                                      )
                                                                    ]
                                                                    fUnsafeFromDataValue
                                                                  ]
                                                                  [
                                                                    {
                                                                      (builtin
                                                                        headList
                                                                      )
                                                                      (con data)
                                                                    }
                                                                    t
                                                                  ]
                                                                ]
                                                              ]
                                                              [
                                                                [
                                                                  {
                                                                    fUnsafeFromDataNil_cunsafeFromBuiltinData
                                                                    DCert
                                                                  }
                                                                  fUnsafeFromDataDCert_cunsafeFromBuiltinData
                                                                ]
                                                                [
                                                                  {
                                                                    (builtin
                                                                      headList
                                                                    )
                                                                    (con data)
                                                                  }
                                                                  t
                                                                ]
                                                              ]
                                                            ]
                                                            [
                                                              [
                                                                {
                                                                  fUnsafeFromDataNil_cunsafeFromBuiltinData
                                                                  [
                                                                    [
                                                                      Tuple2
                                                                      StakingCredential
                                                                    ]
                                                                    (con
                                                                      integer
                                                                    )
                                                                  ]
                                                                }
                                                                [
                                                                  [
                                                                    {
                                                                      {
                                                                        fUnsafeFromDataTuple2_cunsafeFromBuiltinData
                                                                        StakingCredential
                                                                      }
                                                                      (con
                                                                        integer
                                                                      )
                                                                    }
                                                                    fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData
                                                                  ]
                                                                  (builtin
                                                                    unIData
                                                                  )
                                                                ]
                                                              ]
                                                              [
                                                                {
                                                                  (builtin
                                                                    headList
                                                                  )
                                                                  (con data)
                                                                }
                                                                t
                                                              ]
                                                            ]
                                                          ]
                                                          (let
                                                            (nonrec)
                                                            (termbind
                                                              (strict)
                                                              (vardecl
                                                                d (con data)
                                                              )
                                                              [
                                                                {
                                                                  (builtin
                                                                    headList
                                                                  )
                                                                  (con data)
                                                                }
                                                                t
                                                              ]
                                                            )
                                                            (termbind
                                                              (nonstrict)
                                                              (vardecl
                                                                tup
                                                                [
                                                                  [
                                                                    (con pair)
                                                                    (con
                                                                      integer
                                                                    )
                                                                  ]
                                                                  [
                                                                    (con list)
                                                                    (con data)
                                                                  ]
                                                                ]
                                                              )
                                                              [
                                                                (builtin
                                                                  unConstrData
                                                                )
                                                                d
                                                              ]
                                                            )
                                                            (termbind
                                                              (nonstrict)
                                                              (vardecl
                                                                t
                                                                [
                                                                  (con list)
                                                                  (con data)
                                                                ]
                                                              )
                                                              [
                                                                {
                                                                  {
                                                                    (builtin
                                                                      sndPair
                                                                    )
                                                                    (con
                                                                      integer
                                                                    )
                                                                  }
                                                                  [
                                                                    (con list)
                                                                    (con data)
                                                                  ]
                                                                }
                                                                tup
                                                              ]
                                                            )
                                                            [
                                                              [
                                                                [
                                                                  [
                                                                    {
                                                                      (builtin
                                                                        ifThenElse
                                                                      )
                                                                      (fun
                                                                        (con
                                                                          unit
                                                                        )
                                                                        [
                                                                          Interval
                                                                          (con
                                                                            integer
                                                                          )
                                                                        ]
                                                                      )
                                                                    }
                                                                    [
                                                                      [
                                                                        (builtin
                                                                          equalsInteger
                                                                        )
                                                                        [
                                                                          {
                                                                            {
                                                                              (builtin
                                                                                fstPair
                                                                              )
                                                                              (con
                                                                                integer
                                                                              )
                                                                            }
                                                                            [
                                                                              (con
                                                                                list
                                                                              )
                                                                              (con
                                                                                data
                                                                              )
                                                                            ]
                                                                          }
                                                                          tup
                                                                        ]
                                                                      ]
                                                                      (con
                                                                        integer
                                                                        0
                                                                      )
                                                                    ]
                                                                  ]
                                                                  (lam
                                                                    ds
                                                                    (con unit)
                                                                    [
                                                                      [
                                                                        {
                                                                          Interval
                                                                          (con
                                                                            integer
                                                                          )
                                                                        }
                                                                        (let
                                                                          (nonrec)
                                                                          (termbind
                                                                            (strict)
                                                                            (vardecl
                                                                              d
                                                                              (con
                                                                                data
                                                                              )
                                                                            )
                                                                            [
                                                                              {
                                                                                (builtin
                                                                                  headList
                                                                                )
                                                                                (con
                                                                                  data
                                                                                )
                                                                              }
                                                                              t
                                                                            ]
                                                                          )
                                                                          (termbind
                                                                            (nonstrict)
                                                                            (vardecl
                                                                              tup
                                                                              [
                                                                                [
                                                                                  (con
                                                                                    pair
                                                                                  )
                                                                                  (con
                                                                                    integer
                                                                                  )
                                                                                ]
                                                                                [
                                                                                  (con
                                                                                    list
                                                                                  )
                                                                                  (con
                                                                                    data
                                                                                  )
                                                                                ]
                                                                              ]
                                                                            )
                                                                            [
                                                                              (builtin
                                                                                unConstrData
                                                                              )
                                                                              d
                                                                            ]
                                                                          )
                                                                          (termbind
                                                                            (nonstrict)
                                                                            (vardecl
                                                                              t
                                                                              [
                                                                                (con
                                                                                  list
                                                                                )
                                                                                (con
                                                                                  data
                                                                                )
                                                                              ]
                                                                            )
                                                                            [
                                                                              {
                                                                                {
                                                                                  (builtin
                                                                                    sndPair
                                                                                  )
                                                                                  (con
                                                                                    integer
                                                                                  )
                                                                                }
                                                                                [
                                                                                  (con
                                                                                    list
                                                                                  )
                                                                                  (con
                                                                                    data
                                                                                  )
                                                                                ]
                                                                              }
                                                                              tup
                                                                            ]
                                                                          )
                                                                          [
                                                                            [
                                                                              [
                                                                                [
                                                                                  {
                                                                                    (builtin
                                                                                      ifThenElse
                                                                                    )
                                                                                    (fun
                                                                                      (con
                                                                                        unit
                                                                                      )
                                                                                      [
                                                                                        LowerBound
                                                                                        (con
                                                                                          integer
                                                                                        )
                                                                                      ]
                                                                                    )
                                                                                  }
                                                                                  [
                                                                                    [
                                                                                      (builtin
                                                                                        equalsInteger
                                                                                      )
                                                                                      [
                                                                                        {
                                                                                          {
                                                                                            (builtin
                                                                                              fstPair
                                                                                            )
                                                                                            (con
                                                                                              integer
                                                                                            )
                                                                                          }
                                                                                          [
                                                                                            (con
                                                                                              list
                                                                                            )
                                                                                            (con
                                                                                              data
                                                                                            )
                                                                                          ]
                                                                                        }
                                                                                        tup
                                                                                      ]
                                                                                    ]
                                                                                    (con
                                                                                      integer
                                                                                      0
                                                                                    )
                                                                                  ]
                                                                                ]
                                                                                (lam
                                                                                  ds
                                                                                  (con
                                                                                    unit
                                                                                  )
                                                                                  [
                                                                                    [
                                                                                      {
                                                                                        LowerBound
                                                                                        (con
                                                                                          integer
                                                                                        )
                                                                                      }
                                                                                      [
                                                                                        [
                                                                                          {
                                                                                            fUnsafeFromDataExtended_cunsafeFromBuiltinData
                                                                                            (con
                                                                                              integer
                                                                                            )
                                                                                          }
                                                                                          (builtin
                                                                                            unIData
                                                                                          )
                                                                                        ]
                                                                                        [
                                                                                          {
                                                                                            (builtin
                                                                                              headList
                                                                                            )
                                                                                            (con
                                                                                              data
                                                                                            )
                                                                                          }
                                                                                          t
                                                                                        ]
                                                                                      ]
                                                                                    ]
                                                                                    [
                                                                                      fUnsafeFromDataBool_cunsafeFromBuiltinData
                                                                                      [
                                                                                        {
                                                                                          (builtin
                                                                                            headList
                                                                                          )
                                                                                          (con
                                                                                            data
                                                                                          )
                                                                                        }
                                                                                        [
                                                                                          {
                                                                                            (builtin
                                                                                              tailList
                                                                                            )
                                                                                            (con
                                                                                              data
                                                                                            )
                                                                                          }
                                                                                          t
                                                                                        ]
                                                                                      ]
                                                                                    ]
                                                                                  ]
                                                                                )
                                                                              ]
                                                                              (lam
                                                                                ds
                                                                                (con
                                                                                  unit
                                                                                )
                                                                                (let
                                                                                  (nonrec)
                                                                                  (termbind
                                                                                    (strict)
                                                                                    (vardecl
                                                                                      thunk
                                                                                      (con
                                                                                        unit
                                                                                      )
                                                                                    )
                                                                                    [
                                                                                      {
                                                                                        [
                                                                                          Unit_match
                                                                                          [
                                                                                            [
                                                                                              {
                                                                                                (builtin
                                                                                                  trace
                                                                                                )
                                                                                                Unit
                                                                                              }
                                                                                              reconstructCaseError
                                                                                            ]
                                                                                            Unit
                                                                                          ]
                                                                                        ]
                                                                                        (con
                                                                                          unit
                                                                                        )
                                                                                      }
                                                                                      unitval
                                                                                    ]
                                                                                  )
                                                                                  (error
                                                                                    [
                                                                                      LowerBound
                                                                                      (con
                                                                                        integer
                                                                                      )
                                                                                    ]
                                                                                  )
                                                                                )
                                                                              )
                                                                            ]
                                                                            unitval
                                                                          ]
                                                                        )
                                                                      ]
                                                                      (let
                                                                        (nonrec)
                                                                        (termbind
                                                                          (strict)
                                                                          (vardecl
                                                                            d
                                                                            (con
                                                                              data
                                                                            )
                                                                          )
                                                                          [
                                                                            {
                                                                              (builtin
                                                                                headList
                                                                              )
                                                                              (con
                                                                                data
                                                                              )
                                                                            }
                                                                            [
                                                                              {
                                                                                (builtin
                                                                                  tailList
                                                                                )
                                                                                (con
                                                                                  data
                                                                                )
                                                                              }
                                                                              t
                                                                            ]
                                                                          ]
                                                                        )
                                                                        (termbind
                                                                          (nonstrict)
                                                                          (vardecl
                                                                            tup
                                                                            [
                                                                              [
                                                                                (con
                                                                                  pair
                                                                                )
                                                                                (con
                                                                                  integer
                                                                                )
                                                                              ]
                                                                              [
                                                                                (con
                                                                                  list
                                                                                )
                                                                                (con
                                                                                  data
                                                                                )
                                                                              ]
                                                                            ]
                                                                          )
                                                                          [
                                                                            (builtin
                                                                              unConstrData
                                                                            )
                                                                            d
                                                                          ]
                                                                        )
                                                                        (termbind
                                                                          (nonstrict)
                                                                          (vardecl
                                                                            t
                                                                            [
                                                                              (con
                                                                                list
                                                                              )
                                                                              (con
                                                                                data
                                                                              )
                                                                            ]
                                                                          )
                                                                          [
                                                                            {
                                                                              {
                                                                                (builtin
                                                                                  sndPair
                                                                                )
                                                                                (con
                                                                                  integer
                                                                                )
                                                                              }
                                                                              [
                                                                                (con
                                                                                  list
                                                                                )
                                                                                (con
                                                                                  data
                                                                                )
                                                                              ]
                                                                            }
                                                                            tup
                                                                          ]
                                                                        )
                                                                        [
                                                                          [
                                                                            [
                                                                              [
                                                                                {
                                                                                  (builtin
                                                                                    ifThenElse
                                                                                  )
                                                                                  (fun
                                                                                    (con
                                                                                      unit
                                                                                    )
                                                                                    [
                                                                                      UpperBound
                                                                                      (con
                                                                                        integer
                                                                                      )
                                                                                    ]
                                                                                  )
                                                                                }
                                                                                [
                                                                                  [
                                                                                    (builtin
                                                                                      equalsInteger
                                                                                    )
                                                                                    [
                                                                                      {
                                                                                        {
                                                                                          (builtin
                                                                                            fstPair
                                                                                          )
                                                                                          (con
                                                                                            integer
                                                                                          )
                                                                                        }
                                                                                        [
                                                                                          (con
                                                                                            list
                                                                                          )
                                                                                          (con
                                                                                            data
                                                                                          )
                                                                                        ]
                                                                                      }
                                                                                      tup
                                                                                    ]
                                                                                  ]
                                                                                  (con
                                                                                    integer
                                                                                    0
                                                                                  )
                                                                                ]
                                                                              ]
                                                                              (lam
                                                                                ds
                                                                                (con
                                                                                  unit
                                                                                )
                                                                                [
                                                                                  [
                                                                                    {
                                                                                      UpperBound
                                                                                      (con
                                                                                        integer
                                                                                      )
                                                                                    }
                                                                                    [
                                                                                      [
                                                                                        {
                                                                                          fUnsafeFromDataExtended_cunsafeFromBuiltinData
                                                                                          (con
                                                                                            integer
                                                                                          )
                                                                                        }
                                                                                        (builtin
                                                                                          unIData
                                                                                        )
                                                                                      ]
                                                                                      [
                                                                                        {
                                                                                          (builtin
                                                                                            headList
                                                                                          )
                                                                                          (con
                                                                                            data
                                                                                          )
                                                                                        }
                                                                                        t
                                                                                      ]
                                                                                    ]
                                                                                  ]
                                                                                  [
                                                                                    fUnsafeFromDataBool_cunsafeFromBuiltinData
                                                                                    [
                                                                                      {
                                                                                        (builtin
                                                                                          headList
                                                                                        )
                                                                                        (con
                                                                                          data
                                                                                        )
                                                                                      }
                                                                                      [
                                                                                        {
                                                                                          (builtin
                                                                                            tailList
                                                                                          )
                                                                                          (con
                                                                                            data
                                                                                          )
                                                                                        }
                                                                                        t
                                                                                      ]
                                                                                    ]
                                                                                  ]
                                                                                ]
                                                                              )
                                                                            ]
                                                                            (lam
                                                                              ds
                                                                              (con
                                                                                unit
                                                                              )
                                                                              (let
                                                                                (nonrec)
                                                                                (termbind
                                                                                  (strict)
                                                                                  (vardecl
                                                                                    thunk
                                                                                    (con
                                                                                      unit
                                                                                    )
                                                                                  )
                                                                                  [
                                                                                    {
                                                                                      [
                                                                                        Unit_match
                                                                                        [
                                                                                          [
                                                                                            {
                                                                                              (builtin
                                                                                                trace
                                                                                              )
                                                                                              Unit
                                                                                            }
                                                                                            reconstructCaseError
                                                                                          ]
                                                                                          Unit
                                                                                        ]
                                                                                      ]
                                                                                      (con
                                                                                        unit
                                                                                      )
                                                                                    }
                                                                                    unitval
                                                                                  ]
                                                                                )
                                                                                (error
                                                                                  [
                                                                                    UpperBound
                                                                                    (con
                                                                                      integer
                                                                                    )
                                                                                  ]
                                                                                )
                                                                              )
                                                                            )
                                                                          ]
                                                                          unitval
                                                                        ]
                                                                      )
                                                                    ]
                                                                  )
                                                                ]
                                                                (lam
                                                                  ds
                                                                  (con unit)
                                                                  (let
                                                                    (nonrec)
                                                                    (termbind
                                                                      (strict)
                                                                      (vardecl
                                                                        thunk
                                                                        (con
                                                                          unit
                                                                        )
                                                                      )
                                                                      [
                                                                        {
                                                                          [
                                                                            Unit_match
                                                                            [
                                                                              [
                                                                                {
                                                                                  (builtin
                                                                                    trace
                                                                                  )
                                                                                  Unit
                                                                                }
                                                                                reconstructCaseError
                                                                              ]
                                                                              Unit
                                                                            ]
                                                                          ]
                                                                          (con
                                                                            unit
                                                                          )
                                                                        }
                                                                        unitval
                                                                      ]
                                                                    )
                                                                    (error
                                                                      [
                                                                        Interval
                                                                        (con
                                                                          integer
                                                                        )
                                                                      ]
                                                                    )
                                                                  )
                                                                )
                                                              ]
                                                              unitval
                                                            ]
                                                          )
                                                        ]
                                                        [
                                                          [
                                                            {
                                                              fUnsafeFromDataNil_cunsafeFromBuiltinData
                                                              (con bytestring)
                                                            }
                                                            (builtin unBData)
                                                          ]
                                                          [
                                                            {
                                                              (builtin headList)
                                                              (con data)
                                                            }
                                                            t
                                                          ]
                                                        ]
                                                      ]
                                                      [
                                                        [
                                                          {
                                                            fUnsafeFromDataNil_cunsafeFromBuiltinData
                                                            [
                                                              [
                                                                Tuple2
                                                                (con bytestring)
                                                              ]
                                                              (con data)
                                                            ]
                                                          }
                                                          [
                                                            [
                                                              {
                                                                {
                                                                  fUnsafeFromDataTuple2_cunsafeFromBuiltinData
                                                                  (con
                                                                    bytestring
                                                                  )
                                                                }
                                                                (con data)
                                                              }
                                                              (builtin unBData)
                                                            ]
                                                            (lam d (con data) d)
                                                          ]
                                                        ]
                                                        [
                                                          {
                                                            (builtin headList)
                                                            (con data)
                                                          }
                                                          t
                                                        ]
                                                      ]
                                                    ]
                                                    [
                                                      fUnsafeFromDataTxId_cunsafeFromBuiltinData
                                                      [
                                                        {
                                                          (builtin headList)
                                                          (con data)
                                                        }
                                                        [
                                                          {
                                                            (builtin tailList)
                                                            (con data)
                                                          }
                                                          t
                                                        ]
                                                      ]
                                                    ]
                                                  ]
                                                )
                                              ]
                                              (lam
                                                ds
                                                (con unit)
                                                (let
                                                  (nonrec)
                                                  (termbind
                                                    (strict)
                                                    (vardecl thunk (con unit))
                                                    [
                                                      {
                                                        [
                                                          Unit_match
                                                          [
                                                            [
                                                              {
                                                                (builtin trace)
                                                                Unit
                                                              }
                                                              reconstructCaseError
                                                            ]
                                                            Unit
                                                          ]
                                                        ]
                                                        (con unit)
                                                      }
                                                      unitval
                                                    ]
                                                  )
                                                  (error TxInfo)
                                                )
                                              )
                                            ]
                                            unitval
                                          ]
                                        )
                                      ]
                                      (let
                                        (nonrec)
                                        (termbind
                                          (strict)
                                          (vardecl d (con data))
                                          [
                                            { (builtin headList) (con data) }
                                            [
                                              { (builtin tailList) (con data) }
                                              t
                                            ]
                                          ]
                                        )
                                        (termbind
                                          (nonstrict)
                                          (vardecl
                                            tup
                                            [
                                              [ (con pair) (con integer) ]
                                              [ (con list) (con data) ]
                                            ]
                                          )
                                          [ (builtin unConstrData) d ]
                                        )
                                        (termbind
                                          (nonstrict)
                                          (vardecl index (con integer))
                                          [
                                            {
                                              {
                                                (builtin fstPair) (con integer)
                                              }
                                              [ (con list) (con data) ]
                                            }
                                            tup
                                          ]
                                        )
                                        [
                                          [
                                            [
                                              [
                                                {
                                                  (builtin ifThenElse)
                                                  (fun (con unit) ScriptPurpose)
                                                }
                                                [
                                                  [
                                                    (builtin equalsInteger)
                                                    index
                                                  ]
                                                  (con integer 3)
                                                ]
                                              ]
                                              (lam
                                                ds
                                                (con unit)
                                                [
                                                  Certifying
                                                  [
                                                    fUnsafeFromDataDCert_cunsafeFromBuiltinData
                                                    [
                                                      {
                                                        (builtin headList)
                                                        (con data)
                                                      }
                                                      [
                                                        {
                                                          {
                                                            (builtin sndPair)
                                                            (con integer)
                                                          }
                                                          [
                                                            (con list)
                                                            (con data)
                                                          ]
                                                        }
                                                        tup
                                                      ]
                                                    ]
                                                  ]
                                                ]
                                              )
                                            ]
                                            (lam
                                              ds
                                              (con unit)
                                              [
                                                [
                                                  [
                                                    [
                                                      {
                                                        (builtin ifThenElse)
                                                        (fun
                                                          (con unit)
                                                          ScriptPurpose
                                                        )
                                                      }
                                                      [
                                                        [
                                                          (builtin
                                                            equalsInteger
                                                          )
                                                          index
                                                        ]
                                                        (con integer 2)
                                                      ]
                                                    ]
                                                    (lam
                                                      ds
                                                      (con unit)
                                                      [
                                                        Rewarding
                                                        [
                                                          fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData
                                                          [
                                                            {
                                                              (builtin headList)
                                                              (con data)
                                                            }
                                                            [
                                                              {
                                                                {
                                                                  (builtin
                                                                    sndPair
                                                                  )
                                                                  (con integer)
                                                                }
                                                                [
                                                                  (con list)
                                                                  (con data)
                                                                ]
                                                              }
                                                              tup
                                                            ]
                                                          ]
                                                        ]
                                                      ]
                                                    )
                                                  ]
                                                  (lam
                                                    ds
                                                    (con unit)
                                                    [
                                                      [
                                                        [
                                                          [
                                                            {
                                                              (builtin
                                                                ifThenElse
                                                              )
                                                              (fun
                                                                (con unit)
                                                                ScriptPurpose
                                                              )
                                                            }
                                                            [
                                                              [
                                                                (builtin
                                                                  equalsInteger
                                                                )
                                                                index
                                                              ]
                                                              (con integer 1)
                                                            ]
                                                          ]
                                                          (lam
                                                            ds
                                                            (con unit)
                                                            [
                                                              Spending
                                                              [
                                                                fUnsafeFromDataTxOutRef_cunsafeFromBuiltinData
                                                                [
                                                                  {
                                                                    (builtin
                                                                      headList
                                                                    )
                                                                    (con data)
                                                                  }
                                                                  [
                                                                    {
                                                                      {
                                                                        (builtin
                                                                          sndPair
                                                                        )
                                                                        (con
                                                                          integer
                                                                        )
                                                                      }
                                                                      [
                                                                        (con
                                                                          list
                                                                        )
                                                                        (con
                                                                          data
                                                                        )
                                                                      ]
                                                                    }
                                                                    tup
                                                                  ]
                                                                ]
                                                              ]
                                                            ]
                                                          )
                                                        ]
                                                        (lam
                                                          ds
                                                          (con unit)
                                                          [
                                                            [
                                                              [
                                                                [
                                                                  {
                                                                    (builtin
                                                                      ifThenElse
                                                                    )
                                                                    (fun
                                                                      (con unit)
                                                                      ScriptPurpose
                                                                    )
                                                                  }
                                                                  [
                                                                    [
                                                                      (builtin
                                                                        equalsInteger
                                                                      )
                                                                      index
                                                                    ]
                                                                    (con
                                                                      integer 0
                                                                    )
                                                                  ]
                                                                ]
                                                                (lam
                                                                  ds
                                                                  (con unit)
                                                                  [
                                                                    Minting
                                                                    [
                                                                      (builtin
                                                                        unBData
                                                                      )
                                                                      [
                                                                        {
                                                                          (builtin
                                                                            headList
                                                                          )
                                                                          (con
                                                                            data
                                                                          )
                                                                        }
                                                                        [
                                                                          {
                                                                            {
                                                                              (builtin
                                                                                sndPair
                                                                              )
                                                                              (con
                                                                                integer
                                                                              )
                                                                            }
                                                                            [
                                                                              (con
                                                                                list
                                                                              )
                                                                              (con
                                                                                data
                                                                              )
                                                                            ]
                                                                          }
                                                                          tup
                                                                        ]
                                                                      ]
                                                                    ]
                                                                  ]
                                                                )
                                                              ]
                                                              (lam
                                                                ds
                                                                (con unit)
                                                                (let
                                                                  (nonrec)
                                                                  (termbind
                                                                    (strict)
                                                                    (vardecl
                                                                      thunk
                                                                      (con unit)
                                                                    )
                                                                    [
                                                                      {
                                                                        [
                                                                          Unit_match
                                                                          [
                                                                            [
                                                                              {
                                                                                (builtin
                                                                                  trace
                                                                                )
                                                                                Unit
                                                                              }
                                                                              reconstructCaseError
                                                                            ]
                                                                            Unit
                                                                          ]
                                                                        ]
                                                                        (con
                                                                          unit
                                                                        )
                                                                      }
                                                                      unitval
                                                                    ]
                                                                  )
                                                                  (error
                                                                    ScriptPurpose
                                                                  )
                                                                )
                                                              )
                                                            ]
                                                            unitval
                                                          ]
                                                        )
                                                      ]
                                                      unitval
                                                    ]
                                                  )
                                                ]
                                                unitval
                                              ]
                                            )
                                          ]
                                          unitval
                                        ]
                                      )
                                    ]
                                  )
                                ]
                                (lam
                                  ds
                                  (con unit)
                                  (let
                                    (nonrec)
                                    (termbind
                                      (strict)
                                      (vardecl thunk (con unit))
                                      [
                                        {
                                          [
                                            Unit_match
                                            [
                                              [
                                                { (builtin trace) Unit }
                                                reconstructCaseError
                                              ]
                                              Unit
                                            ]
                                          ]
                                          (con unit)
                                        }
                                        unitval
                                      ]
                                    )
                                    (error ScriptContext)
                                  )
                                )
                              ]
                              unitval
                            ]
                          )
                          [ { [ Unit_match ds ] Bool } True ]
                        )
                      ]
                      (all dead (type) Unit)
                    }
                    (abs dead (type) Unit)
                  ]
                  (abs
                    dead
                    (type)
                    (let
                      (nonrec)
                      (termbind
                        (strict)
                        (vardecl thunk (con unit))
                        [
                          {
                            [
                              Unit_match
                              [
                                [ { (builtin trace) Unit } (con string "PT5") ]
                                Unit
                              ]
                            ]
                            (con unit)
                          }
                          unitval
                        ]
                      )
                      (error Unit)
                    )
                  )
                ]
                (all dead (type) dead)
              }
            )
          )
        )
      )
    )
  )
)