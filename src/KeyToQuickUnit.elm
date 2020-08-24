module KeyToQuickUnit exposing (keyToQuickUnit)

import Dict exposing (Dict)

keyToQuickUnit : Dict Char Char
keyToQuickUnit = Dict.fromList
  [
    ('q', '手'), 
    ('w', '田'), 
    ('e', '水'), 
    ('r', '口'), 
    ('t', '廿'), 
    ('y', '卜'), 
    ('u', '山'), 
    ('i', '戈'), 
    ('o', '人'), 
    ('p', '心'), 
    ('a', '日'), 
    ('s', '尸'), 
    ('d', '木'), 
    ('f', '火'), 
    ('g', '土'), 
    ('h', '竹'), 
    ('j', '十'), 
    ('k', '大'), 
    ('l', '中'), 
    ('z', '重'), 
    ('x', '難'), 
    ('c', '金'), 
    ('v', '女'), 
    ('b', '月'), 
    ('n', '弓'), 
    ('m', '一')
  ]