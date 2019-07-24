## IFM Setting - T-CODE: ZFLIFMS

![IFM Setting](https://github.com/beyondnk/Z_FILAB_IFM/blob/master/docs/IFM_Setting.PNG)


1. COA Setting: Enter COA that is the basis for FSV you are using.

+ __This current version does not support group COA.__

2. FSV Setting: Enter FSV you are using.

3. Integrated Currency key: Enter the currency key that you want to collectively evaluate.

4. To Period Setting: The reporting display period.

## FSV Maintenance - T-CODE: ZFLIFMV_LAYOUT

![IFM Maintenance](https://github.com/open-fi-lab/Z_FILAB_IFM/blob/master/docs/IFM_FSV.PNG)

Enter an item keys for each value from FSE3


T-Code: FSE3

![IFM Maintenance](https://github.com/open-fi-lab/Z_FILAB_IFM/blob/master/docs/FSV.PNG)

- If you want to see the item keys, go to Menu: Settings-Change.
- Check Item keys visible.
- If 'Item keys visible' is disable, your fsv is not automatically allocate item keys.
- Check table T011(Financial Statement Versions). Field: XAUTO

## Exchange Rates - T-CODE: ZFLIFMV_TCURR

![IFM Maintenance](https://github.com/open-fi-lab/Z_FILAB_IFM/blob/master/docs/IFM_Exch.PNG)

- Not required if all companies use the same currency.
- The period value '0' is the exchange rate for one year, and each period is the exchange rate for that period.

## Finish

Run! ZFLIFMR

