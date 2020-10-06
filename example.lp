\* Problem: Unknown *\

Minimize
 obj: + "C_max"

Subject To
 r_1: + 2 "x_Task_(TaskID_102)_(B_3)_Resource_(RID_3)_RB"
 + 2 "Theta_Task_(TaskID_102)_(B_3)_Task_(TaskID_102)_(B_3)" <= 3
 r_2: + "x_Task_(TaskID_102)_(B_3)_Resource_(RID_3)_RB"
 + "x_Task_(TaskID_101)_(B_3)_Resource_(RID_3)_RB"
 + "Theta_Task_(TaskID_102)_(B_3)_Task_(TaskID_101)_(B_3)"
 + "Theta_Task_(TaskID_101)_(B_3)_Task_(TaskID_102)_(B_3)" <= 3
 r_3: + "x_Task_(TaskID_102)_(B_3)_Resource_(RID_3)_RB"
 + "x_Task_(TaskID_101)_(B_3)_Resource_(RID_3)_RB"
 + "Theta_Task_(TaskID_102)_(B_3)_Task_(TaskID_101)_(B_3)"
 + "Theta_Task_(TaskID_101)_(B_3)_Task_(TaskID_102)_(B_3)" <= 3
 r_4: + 2 "x_Task_(TaskID_101)_(B_3)_Resource_(RID_3)_RB"
 + 2 "Theta_Task_(TaskID_101)_(B_3)_Task_(TaskID_101)_(B_3)" <= 3
 r_5: + 2 "x_Task_(TaskID_4)_A_Resource_(RID_2)_RA"
 + 2 "Theta_Task_(TaskID_4)_A_Task_(TaskID_4)_A" <= 3
 r_6: + 2 "x_Task_(TaskID_4)_A_Resource_(RID_1)_RA"
 + 2 "Theta_Task_(TaskID_4)_A_Task_(TaskID_4)_A" <= 3
 r_7: + "x_Task_(TaskID_4)_A_Resource_(RID_2)_RA"
 + "x_Task_(TaskID_3)_A_Resource_(RID_2)_RA"
 + "Theta_Task_(TaskID_4)_A_Task_(TaskID_3)_A"
 + "Theta_Task_(TaskID_3)_A_Task_(TaskID_4)_A" <= 3
 r_8: + "x_Task_(TaskID_4)_A_Resource_(RID_1)_RA"
 + "x_Task_(TaskID_3)_A_Resource_(RID_1)_RA"
 + "Theta_Task_(TaskID_4)_A_Task_(TaskID_3)_A"
 + "Theta_Task_(TaskID_3)_A_Task_(TaskID_4)_A" <= 3
 r_9: + "x_Task_(TaskID_4)_A_Resource_(RID_2)_RA"
 + "x_Task_(TaskID_2)_A_Resource_(RID_2)_RA"
 + "Theta_Task_(TaskID_4)_A_Task_(TaskID_2)_A"
 + "Theta_Task_(TaskID_2)_A_Task_(TaskID_4)_A" <= 3
 r_10: + "x_Task_(TaskID_4)_A_Resource_(RID_1)_RA"
 + "x_Task_(TaskID_2)_A_Resource_(RID_1)_RA"
 + "Theta_Task_(TaskID_4)_A_Task_(TaskID_2)_A"
 + "Theta_Task_(TaskID_2)_A_Task_(TaskID_4)_A" <= 3
 r_11: + "x_Task_(TaskID_4)_A_Resource_(RID_2)_RA"
 + "x_Task_(TaskID_1)_A_Resource_(RID_2)_RA"
 + "Theta_Task_(TaskID_4)_A_Task_(TaskID_1)_A"
 + "Theta_Task_(TaskID_1)_A_Task_(TaskID_4)_A" <= 3
 r_12: + "x_Task_(TaskID_4)_A_Resource_(RID_1)_RA"
 + "x_Task_(TaskID_1)_A_Resource_(RID_1)_RA"
 + "Theta_Task_(TaskID_4)_A_Task_(TaskID_1)_A"
 + "Theta_Task_(TaskID_1)_A_Task_(TaskID_4)_A" <= 3
 r_13: + "x_Task_(TaskID_4)_A_Resource_(RID_2)_RA"
 + "x_Task_(TaskID_3)_A_Resource_(RID_2)_RA"
 + "Theta_Task_(TaskID_4)_A_Task_(TaskID_3)_A"
 + "Theta_Task_(TaskID_3)_A_Task_(TaskID_4)_A" <= 3
 r_14: + "x_Task_(TaskID_4)_A_Resource_(RID_1)_RA"
 + "x_Task_(TaskID_3)_A_Resource_(RID_1)_RA"
 + "Theta_Task_(TaskID_4)_A_Task_(TaskID_3)_A"
 + "Theta_Task_(TaskID_3)_A_Task_(TaskID_4)_A" <= 3
 r_15: + 2 "x_Task_(TaskID_3)_A_Resource_(RID_2)_RA"
 + 2 "Theta_Task_(TaskID_3)_A_Task_(TaskID_3)_A" <= 3
 r_16: + 2 "x_Task_(TaskID_3)_A_Resource_(RID_1)_RA"
 + 2 "Theta_Task_(TaskID_3)_A_Task_(TaskID_3)_A" <= 3
 r_17: + "x_Task_(TaskID_3)_A_Resource_(RID_2)_RA"
 + "x_Task_(TaskID_2)_A_Resource_(RID_2)_RA"
 + "Theta_Task_(TaskID_3)_A_Task_(TaskID_2)_A"
 + "Theta_Task_(TaskID_2)_A_Task_(TaskID_3)_A" <= 3
 r_18: + "x_Task_(TaskID_3)_A_Resource_(RID_1)_RA"
 + "x_Task_(TaskID_2)_A_Resource_(RID_1)_RA"
 + "Theta_Task_(TaskID_3)_A_Task_(TaskID_2)_A"
 + "Theta_Task_(TaskID_2)_A_Task_(TaskID_3)_A" <= 3
 r_19: + "x_Task_(TaskID_3)_A_Resource_(RID_2)_RA"
 + "x_Task_(TaskID_1)_A_Resource_(RID_2)_RA"
 + "Theta_Task_(TaskID_3)_A_Task_(TaskID_1)_A"
 + "Theta_Task_(TaskID_1)_A_Task_(TaskID_3)_A" <= 3
 r_20: + "x_Task_(TaskID_3)_A_Resource_(RID_1)_RA"
 + "x_Task_(TaskID_1)_A_Resource_(RID_1)_RA"
 + "Theta_Task_(TaskID_3)_A_Task_(TaskID_1)_A"
 + "Theta_Task_(TaskID_1)_A_Task_(TaskID_3)_A" <= 3
 r_21: + "x_Task_(TaskID_4)_A_Resource_(RID_2)_RA"
 + "x_Task_(TaskID_2)_A_Resource_(RID_2)_RA"
 + "Theta_Task_(TaskID_4)_A_Task_(TaskID_2)_A"
 + "Theta_Task_(TaskID_2)_A_Task_(TaskID_4)_A" <= 3
 r_22: + "x_Task_(TaskID_4)_A_Resource_(RID_1)_RA"
 + "x_Task_(TaskID_2)_A_Resource_(RID_1)_RA"
 + "Theta_Task_(TaskID_4)_A_Task_(TaskID_2)_A"
 + "Theta_Task_(TaskID_2)_A_Task_(TaskID_4)_A" <= 3
 r_23: + "x_Task_(TaskID_3)_A_Resource_(RID_2)_RA"
 + "x_Task_(TaskID_2)_A_Resource_(RID_2)_RA"
 + "Theta_Task_(TaskID_3)_A_Task_(TaskID_2)_A"
 + "Theta_Task_(TaskID_2)_A_Task_(TaskID_3)_A" <= 3
 r_24: + "x_Task_(TaskID_3)_A_Resource_(RID_1)_RA"
 + "x_Task_(TaskID_2)_A_Resource_(RID_1)_RA"
 + "Theta_Task_(TaskID_3)_A_Task_(TaskID_2)_A"
 + "Theta_Task_(TaskID_2)_A_Task_(TaskID_3)_A" <= 3
 r_25: + 2 "x_Task_(TaskID_2)_A_Resource_(RID_2)_RA"
 + 2 "Theta_Task_(TaskID_2)_A_Task_(TaskID_2)_A" <= 3
 r_26: + 2 "x_Task_(TaskID_2)_A_Resource_(RID_1)_RA"
 + 2 "Theta_Task_(TaskID_2)_A_Task_(TaskID_2)_A" <= 3
 r_27: + "x_Task_(TaskID_2)_A_Resource_(RID_2)_RA"
 + "x_Task_(TaskID_1)_A_Resource_(RID_2)_RA"
 + "Theta_Task_(TaskID_2)_A_Task_(TaskID_1)_A"
 + "Theta_Task_(TaskID_1)_A_Task_(TaskID_2)_A" <= 3
 r_28: + "x_Task_(TaskID_2)_A_Resource_(RID_1)_RA"
 + "x_Task_(TaskID_1)_A_Resource_(RID_1)_RA"
 + "Theta_Task_(TaskID_2)_A_Task_(TaskID_1)_A"
 + "Theta_Task_(TaskID_1)_A_Task_(TaskID_2)_A" <= 3
 r_29: + "x_Task_(TaskID_4)_A_Resource_(RID_2)_RA"
 + "x_Task_(TaskID_1)_A_Resource_(RID_2)_RA"
 + "Theta_Task_(TaskID_4)_A_Task_(TaskID_1)_A"
 + "Theta_Task_(TaskID_1)_A_Task_(TaskID_4)_A" <= 3
 r_30: + "x_Task_(TaskID_4)_A_Resource_(RID_1)_RA"
 + "x_Task_(TaskID_1)_A_Resource_(RID_1)_RA"
 + "Theta_Task_(TaskID_4)_A_Task_(TaskID_1)_A"
 + "Theta_Task_(TaskID_1)_A_Task_(TaskID_4)_A" <= 3
 r_31: + "x_Task_(TaskID_3)_A_Resource_(RID_2)_RA"
 + "x_Task_(TaskID_1)_A_Resource_(RID_2)_RA"
 + "Theta_Task_(TaskID_3)_A_Task_(TaskID_1)_A"
 + "Theta_Task_(TaskID_1)_A_Task_(TaskID_3)_A" <= 3
 r_32: + "x_Task_(TaskID_3)_A_Resource_(RID_1)_RA"
 + "x_Task_(TaskID_1)_A_Resource_(RID_1)_RA"
 + "Theta_Task_(TaskID_3)_A_Task_(TaskID_1)_A"
 + "Theta_Task_(TaskID_1)_A_Task_(TaskID_3)_A" <= 3
 r_33: + "x_Task_(TaskID_2)_A_Resource_(RID_2)_RA"
 + "x_Task_(TaskID_1)_A_Resource_(RID_2)_RA"
 + "Theta_Task_(TaskID_2)_A_Task_(TaskID_1)_A"
 + "Theta_Task_(TaskID_1)_A_Task_(TaskID_2)_A" <= 3
 r_34: + "x_Task_(TaskID_2)_A_Resource_(RID_1)_RA"
 + "x_Task_(TaskID_1)_A_Resource_(RID_1)_RA"
 + "Theta_Task_(TaskID_2)_A_Task_(TaskID_1)_A"
 + "Theta_Task_(TaskID_1)_A_Task_(TaskID_2)_A" <= 3
 r_35: + 2 "x_Task_(TaskID_1)_A_Resource_(RID_2)_RA"
 + 2 "Theta_Task_(TaskID_1)_A_Task_(TaskID_1)_A" <= 3
 r_36: + 2 "x_Task_(TaskID_1)_A_Resource_(RID_1)_RA"
 + 2 "Theta_Task_(TaskID_1)_A_Task_(TaskID_1)_A" <= 3
 r_37: - 3 "x_Task_(TaskID_102)_(B_3)_Resource_(RID_3)_RB"
 - "s_Task_(TaskID_102)_(B_3)" + "s_Task_(TaskID_101)_(B_3)"
 + 200 "Theta_Task_(TaskID_102)_(B_3)_Task_(TaskID_101)_(B_3)" <= 199
 r_38: - 3 "x_Task_(TaskID_102)_(B_3)_Resource_(RID_3)_RB"
 - "s_Task_(TaskID_102)_(B_3)" + "s_Task_(TaskID_101)_(B_3)"
 + 200 "Theta_Task_(TaskID_102)_(B_3)_Task_(TaskID_101)_(B_3)" >= 0
 r_39: - 3 "x_Task_(TaskID_102)_(B_3)_Resource_(RID_3)_RB"
 + "s_Task_(TaskID_4)_A" - "s_Task_(TaskID_102)_(B_3)"
 + 200 "Theta_Task_(TaskID_102)_(B_3)_Task_(TaskID_4)_A" <= 199
 r_40: - 3 "x_Task_(TaskID_102)_(B_3)_Resource_(RID_3)_RB"
 + "s_Task_(TaskID_4)_A" - "s_Task_(TaskID_102)_(B_3)"
 + 200 "Theta_Task_(TaskID_102)_(B_3)_Task_(TaskID_4)_A" >= 0
 r_41: - 3 "x_Task_(TaskID_102)_(B_3)_Resource_(RID_3)_RB"
 + "s_Task_(TaskID_3)_A" - "s_Task_(TaskID_102)_(B_3)"
 + 200 "Theta_Task_(TaskID_102)_(B_3)_Task_(TaskID_3)_A" <= 199
 r_42: - 3 "x_Task_(TaskID_102)_(B_3)_Resource_(RID_3)_RB"
 + "s_Task_(TaskID_3)_A" - "s_Task_(TaskID_102)_(B_3)"
 + 200 "Theta_Task_(TaskID_102)_(B_3)_Task_(TaskID_3)_A" >= 0
 r_43: - 3 "x_Task_(TaskID_102)_(B_3)_Resource_(RID_3)_RB"
 + "s_Task_(TaskID_2)_A" - "s_Task_(TaskID_102)_(B_3)"
 + 200 "Theta_Task_(TaskID_102)_(B_3)_Task_(TaskID_2)_A" <= 199
 r_44: - 3 "x_Task_(TaskID_102)_(B_3)_Resource_(RID_3)_RB"
 + "s_Task_(TaskID_2)_A" - "s_Task_(TaskID_102)_(B_3)"
 + 200 "Theta_Task_(TaskID_102)_(B_3)_Task_(TaskID_2)_A" >= 0
 r_45: - 3 "x_Task_(TaskID_102)_(B_3)_Resource_(RID_3)_RB"
 - "s_Task_(TaskID_102)_(B_3)" + "s_Task_(TaskID_1)_A"
 + 200 "Theta_Task_(TaskID_102)_(B_3)_Task_(TaskID_1)_A" <= 199
 r_46: - 3 "x_Task_(TaskID_102)_(B_3)_Resource_(RID_3)_RB"
 - "s_Task_(TaskID_102)_(B_3)" + "s_Task_(TaskID_1)_A"
 + 200 "Theta_Task_(TaskID_102)_(B_3)_Task_(TaskID_1)_A" >= 0
 r_47: - 3 "x_Task_(TaskID_101)_(B_3)_Resource_(RID_3)_RB"
 + "s_Task_(TaskID_4)_A" - "s_Task_(TaskID_101)_(B_3)"
 + 200 "Theta_Task_(TaskID_101)_(B_3)_Task_(TaskID_4)_A" <= 199
 r_48: - 3 "x_Task_(TaskID_101)_(B_3)_Resource_(RID_3)_RB"
 + "s_Task_(TaskID_4)_A" - "s_Task_(TaskID_101)_(B_3)"
 + 200 "Theta_Task_(TaskID_101)_(B_3)_Task_(TaskID_4)_A" >= 0
 r_49: - 3 "x_Task_(TaskID_101)_(B_3)_Resource_(RID_3)_RB"
 + "s_Task_(TaskID_3)_A" - "s_Task_(TaskID_101)_(B_3)"
 + 200 "Theta_Task_(TaskID_101)_(B_3)_Task_(TaskID_3)_A" <= 199
 r_50: - 3 "x_Task_(TaskID_101)_(B_3)_Resource_(RID_3)_RB"
 + "s_Task_(TaskID_3)_A" - "s_Task_(TaskID_101)_(B_3)"
 + 200 "Theta_Task_(TaskID_101)_(B_3)_Task_(TaskID_3)_A" >= 0
 r_51: - 3 "x_Task_(TaskID_101)_(B_3)_Resource_(RID_3)_RB"
 - "s_Task_(TaskID_101)_(B_3)" + "s_Task_(TaskID_1)_A"
 + 200 "Theta_Task_(TaskID_101)_(B_3)_Task_(TaskID_1)_A" <= 199
 r_52: - 3 "x_Task_(TaskID_101)_(B_3)_Resource_(RID_3)_RB"
 - "s_Task_(TaskID_101)_(B_3)" + "s_Task_(TaskID_1)_A"
 + 200 "Theta_Task_(TaskID_101)_(B_3)_Task_(TaskID_1)_A" >= 0
 r_53: - "x_Task_(TaskID_4)_A_Resource_(RID_2)_RA"
 - "x_Task_(TaskID_4)_A_Resource_(RID_1)_RA" - "s_Task_(TaskID_4)_A"
 + "s_Task_(TaskID_102)_(B_3)"
 + 200 "Theta_Task_(TaskID_4)_A_Task_(TaskID_102)_(B_3)" <= 199
 r_54: - "x_Task_(TaskID_4)_A_Resource_(RID_2)_RA"
 - "x_Task_(TaskID_4)_A_Resource_(RID_1)_RA" - "s_Task_(TaskID_4)_A"
 + "s_Task_(TaskID_102)_(B_3)"
 + 200 "Theta_Task_(TaskID_4)_A_Task_(TaskID_102)_(B_3)" >= 0
 r_55: - "x_Task_(TaskID_4)_A_Resource_(RID_2)_RA"
 - "x_Task_(TaskID_4)_A_Resource_(RID_1)_RA" - "s_Task_(TaskID_4)_A"
 + "s_Task_(TaskID_101)_(B_3)"
 + 200 "Theta_Task_(TaskID_4)_A_Task_(TaskID_101)_(B_3)" <= 199
 r_56: - "x_Task_(TaskID_4)_A_Resource_(RID_2)_RA"
 - "x_Task_(TaskID_4)_A_Resource_(RID_1)_RA" - "s_Task_(TaskID_4)_A"
 + "s_Task_(TaskID_101)_(B_3)"
 + 200 "Theta_Task_(TaskID_4)_A_Task_(TaskID_101)_(B_3)" >= 0
 r_57: - "x_Task_(TaskID_4)_A_Resource_(RID_2)_RA"
 - "x_Task_(TaskID_4)_A_Resource_(RID_1)_RA" - "s_Task_(TaskID_4)_A"
 + "s_Task_(TaskID_3)_A"
 + 200 "Theta_Task_(TaskID_4)_A_Task_(TaskID_3)_A" <= 199
 r_58: - "x_Task_(TaskID_4)_A_Resource_(RID_2)_RA"
 - "x_Task_(TaskID_4)_A_Resource_(RID_1)_RA" - "s_Task_(TaskID_4)_A"
 + "s_Task_(TaskID_3)_A"
 + 200 "Theta_Task_(TaskID_4)_A_Task_(TaskID_3)_A" >= 0
 r_59: - "x_Task_(TaskID_4)_A_Resource_(RID_2)_RA"
 - "x_Task_(TaskID_4)_A_Resource_(RID_1)_RA" - "s_Task_(TaskID_4)_A"
 + "s_Task_(TaskID_2)_A"
 + 200 "Theta_Task_(TaskID_4)_A_Task_(TaskID_2)_A" <= 199
 r_60: - "x_Task_(TaskID_4)_A_Resource_(RID_2)_RA"
 - "x_Task_(TaskID_4)_A_Resource_(RID_1)_RA" - "s_Task_(TaskID_4)_A"
 + "s_Task_(TaskID_2)_A"
 + 200 "Theta_Task_(TaskID_4)_A_Task_(TaskID_2)_A" >= 0
 r_61: - "x_Task_(TaskID_4)_A_Resource_(RID_2)_RA"
 - "x_Task_(TaskID_4)_A_Resource_(RID_1)_RA" - "s_Task_(TaskID_4)_A"
 + "s_Task_(TaskID_1)_A"
 + 200 "Theta_Task_(TaskID_4)_A_Task_(TaskID_1)_A" <= 199
 r_62: - "x_Task_(TaskID_4)_A_Resource_(RID_2)_RA"
 - "x_Task_(TaskID_4)_A_Resource_(RID_1)_RA" - "s_Task_(TaskID_4)_A"
 + "s_Task_(TaskID_1)_A"
 + 200 "Theta_Task_(TaskID_4)_A_Task_(TaskID_1)_A" >= 0
 r_63: - "x_Task_(TaskID_3)_A_Resource_(RID_2)_RA"
 - "x_Task_(TaskID_3)_A_Resource_(RID_1)_RA" - "s_Task_(TaskID_3)_A"
 + "s_Task_(TaskID_102)_(B_3)"
 + 200 "Theta_Task_(TaskID_3)_A_Task_(TaskID_102)_(B_3)" <= 199
 r_64: - "x_Task_(TaskID_3)_A_Resource_(RID_2)_RA"
 - "x_Task_(TaskID_3)_A_Resource_(RID_1)_RA" - "s_Task_(TaskID_3)_A"
 + "s_Task_(TaskID_102)_(B_3)"
 + 200 "Theta_Task_(TaskID_3)_A_Task_(TaskID_102)_(B_3)" >= 0
 r_65: - "x_Task_(TaskID_3)_A_Resource_(RID_2)_RA"
 - "x_Task_(TaskID_3)_A_Resource_(RID_1)_RA" - "s_Task_(TaskID_3)_A"
 + "s_Task_(TaskID_101)_(B_3)"
 + 200 "Theta_Task_(TaskID_3)_A_Task_(TaskID_101)_(B_3)" <= 199
 r_66: - "x_Task_(TaskID_3)_A_Resource_(RID_2)_RA"
 - "x_Task_(TaskID_3)_A_Resource_(RID_1)_RA" - "s_Task_(TaskID_3)_A"
 + "s_Task_(TaskID_101)_(B_3)"
 + 200 "Theta_Task_(TaskID_3)_A_Task_(TaskID_101)_(B_3)" >= 0
 r_67: - "x_Task_(TaskID_3)_A_Resource_(RID_2)_RA"
 - "x_Task_(TaskID_3)_A_Resource_(RID_1)_RA" + "s_Task_(TaskID_4)_A"
 - "s_Task_(TaskID_3)_A"
 + 200 "Theta_Task_(TaskID_3)_A_Task_(TaskID_4)_A" <= 199
 r_68: - "x_Task_(TaskID_3)_A_Resource_(RID_2)_RA"
 - "x_Task_(TaskID_3)_A_Resource_(RID_1)_RA" + "s_Task_(TaskID_4)_A"
 - "s_Task_(TaskID_3)_A"
 + 200 "Theta_Task_(TaskID_3)_A_Task_(TaskID_4)_A" >= 0
 r_69: - "x_Task_(TaskID_3)_A_Resource_(RID_2)_RA"
 - "x_Task_(TaskID_3)_A_Resource_(RID_1)_RA" - "s_Task_(TaskID_3)_A"
 + "s_Task_(TaskID_2)_A"
 + 200 "Theta_Task_(TaskID_3)_A_Task_(TaskID_2)_A" <= 199
 r_70: - "x_Task_(TaskID_3)_A_Resource_(RID_2)_RA"
 - "x_Task_(TaskID_3)_A_Resource_(RID_1)_RA" - "s_Task_(TaskID_3)_A"
 + "s_Task_(TaskID_2)_A"
 + 200 "Theta_Task_(TaskID_3)_A_Task_(TaskID_2)_A" >= 0
 r_71: - "x_Task_(TaskID_3)_A_Resource_(RID_2)_RA"
 - "x_Task_(TaskID_3)_A_Resource_(RID_1)_RA" - "s_Task_(TaskID_3)_A"
 + "s_Task_(TaskID_1)_A"
 + 200 "Theta_Task_(TaskID_3)_A_Task_(TaskID_1)_A" <= 199
 r_72: - "x_Task_(TaskID_3)_A_Resource_(RID_2)_RA"
 - "x_Task_(TaskID_3)_A_Resource_(RID_1)_RA" - "s_Task_(TaskID_3)_A"
 + "s_Task_(TaskID_1)_A"
 + 200 "Theta_Task_(TaskID_3)_A_Task_(TaskID_1)_A" >= 0
 r_73: - "x_Task_(TaskID_2)_A_Resource_(RID_2)_RA"
 - "x_Task_(TaskID_2)_A_Resource_(RID_1)_RA" - "s_Task_(TaskID_2)_A"
 + "s_Task_(TaskID_101)_(B_3)"
 + 200 "Theta_Task_(TaskID_2)_A_Task_(TaskID_101)_(B_3)" <= 199
 r_74: - "x_Task_(TaskID_2)_A_Resource_(RID_2)_RA"
 - "x_Task_(TaskID_2)_A_Resource_(RID_1)_RA" - "s_Task_(TaskID_2)_A"
 + "s_Task_(TaskID_101)_(B_3)"
 + 200 "Theta_Task_(TaskID_2)_A_Task_(TaskID_101)_(B_3)" >= 0
 r_75: - "x_Task_(TaskID_2)_A_Resource_(RID_2)_RA"
 - "x_Task_(TaskID_2)_A_Resource_(RID_1)_RA" + "s_Task_(TaskID_4)_A"
 - "s_Task_(TaskID_2)_A"
 + 200 "Theta_Task_(TaskID_2)_A_Task_(TaskID_4)_A" <= 199
 r_76: - "x_Task_(TaskID_2)_A_Resource_(RID_2)_RA"
 - "x_Task_(TaskID_2)_A_Resource_(RID_1)_RA" + "s_Task_(TaskID_4)_A"
 - "s_Task_(TaskID_2)_A"
 + 200 "Theta_Task_(TaskID_2)_A_Task_(TaskID_4)_A" >= 0
 r_77: - "x_Task_(TaskID_2)_A_Resource_(RID_2)_RA"
 - "x_Task_(TaskID_2)_A_Resource_(RID_1)_RA" + "s_Task_(TaskID_3)_A"
 - "s_Task_(TaskID_2)_A"
 + 200 "Theta_Task_(TaskID_2)_A_Task_(TaskID_3)_A" <= 199
 r_78: - "x_Task_(TaskID_2)_A_Resource_(RID_2)_RA"
 - "x_Task_(TaskID_2)_A_Resource_(RID_1)_RA" + "s_Task_(TaskID_3)_A"
 - "s_Task_(TaskID_2)_A"
 + 200 "Theta_Task_(TaskID_2)_A_Task_(TaskID_3)_A" >= 0
 r_79: - "x_Task_(TaskID_2)_A_Resource_(RID_2)_RA"
 - "x_Task_(TaskID_2)_A_Resource_(RID_1)_RA" - "s_Task_(TaskID_2)_A"
 + "s_Task_(TaskID_1)_A"
 + 200 "Theta_Task_(TaskID_2)_A_Task_(TaskID_1)_A" <= 199
 r_80: - "x_Task_(TaskID_2)_A_Resource_(RID_2)_RA"
 - "x_Task_(TaskID_2)_A_Resource_(RID_1)_RA" - "s_Task_(TaskID_2)_A"
 + "s_Task_(TaskID_1)_A"
 + 200 "Theta_Task_(TaskID_2)_A_Task_(TaskID_1)_A" >= 0
 r_81: - "x_Task_(TaskID_1)_A_Resource_(RID_2)_RA"
 - "x_Task_(TaskID_1)_A_Resource_(RID_1)_RA" + "s_Task_(TaskID_4)_A"
 - "s_Task_(TaskID_1)_A"
 + 200 "Theta_Task_(TaskID_1)_A_Task_(TaskID_4)_A" <= 199
 r_82: - "x_Task_(TaskID_1)_A_Resource_(RID_2)_RA"
 - "x_Task_(TaskID_1)_A_Resource_(RID_1)_RA" + "s_Task_(TaskID_4)_A"
 - "s_Task_(TaskID_1)_A"
 + 200 "Theta_Task_(TaskID_1)_A_Task_(TaskID_4)_A" >= 0
 r_83: - "x_Task_(TaskID_1)_A_Resource_(RID_2)_RA"
 - "x_Task_(TaskID_1)_A_Resource_(RID_1)_RA" + "s_Task_(TaskID_3)_A"
 - "s_Task_(TaskID_1)_A"
 + 200 "Theta_Task_(TaskID_1)_A_Task_(TaskID_3)_A" <= 199
 r_84: - "x_Task_(TaskID_1)_A_Resource_(RID_2)_RA"
 - "x_Task_(TaskID_1)_A_Resource_(RID_1)_RA" + "s_Task_(TaskID_3)_A"
 - "s_Task_(TaskID_1)_A"
 + 200 "Theta_Task_(TaskID_1)_A_Task_(TaskID_3)_A" >= 0
 r_85: - 3 "x_Task_(TaskID_101)_(B_3)_Resource_(RID_3)_RB"
 + "s_Task_(TaskID_2)_A" - "s_Task_(TaskID_101)_(B_3)" >= 0
 r_86: - "x_Task_(TaskID_2)_A_Resource_(RID_2)_RA"
 - "s_Task_(TaskID_2)_A" + "s_Task_(TaskID_102)_(B_3)" >= 0
 r_87: - "x_Task_(TaskID_2)_A_Resource_(RID_1)_RA"
 - "s_Task_(TaskID_2)_A" + "s_Task_(TaskID_102)_(B_3)" >= 0
 r_88: - "x_Task_(TaskID_1)_A_Resource_(RID_2)_RA"
 + "s_Task_(TaskID_101)_(B_3)" - "s_Task_(TaskID_1)_A" >= 0
 r_89: - "x_Task_(TaskID_1)_A_Resource_(RID_1)_RA"
 + "s_Task_(TaskID_101)_(B_3)" - "s_Task_(TaskID_1)_A" >= 0
 r_90: + "x_Task_(TaskID_102)_(B_3)_Resource_(RID_2)_RA" = 0
 r_91: + "x_Task_(TaskID_102)_(B_3)_Resource_(RID_1)_RA" = 0
 r_92: + "x_Task_(TaskID_101)_(B_3)_Resource_(RID_2)_RA" = 0
 r_93: + "x_Task_(TaskID_101)_(B_3)_Resource_(RID_1)_RA" = 0
 r_94: + "x_Task_(TaskID_4)_A_Resource_(RID_3)_RB" = 0
 r_95: + "x_Task_(TaskID_3)_A_Resource_(RID_3)_RB" = 0
 r_96: + "x_Task_(TaskID_2)_A_Resource_(RID_3)_RB" = 0
 r_97: + "x_Task_(TaskID_1)_A_Resource_(RID_3)_RB" = 0
 r_98: + "x_Task_(TaskID_102)_(B_3)_Resource_(RID_3)_RB"
 + "x_Task_(TaskID_102)_(B_3)_Resource_(RID_2)_RA"
 + "x_Task_(TaskID_102)_(B_3)_Resource_(RID_1)_RA" = 1
 r_99: + "x_Task_(TaskID_101)_(B_3)_Resource_(RID_3)_RB"
 + "x_Task_(TaskID_101)_(B_3)_Resource_(RID_2)_RA"
 + "x_Task_(TaskID_101)_(B_3)_Resource_(RID_1)_RA" = 1
 r_100: + "x_Task_(TaskID_4)_A_Resource_(RID_3)_RB"
 + "x_Task_(TaskID_4)_A_Resource_(RID_2)_RA"
 + "x_Task_(TaskID_4)_A_Resource_(RID_1)_RA" = 1
 r_101: + "x_Task_(TaskID_3)_A_Resource_(RID_3)_RB"
 + "x_Task_(TaskID_3)_A_Resource_(RID_2)_RA"
 + "x_Task_(TaskID_3)_A_Resource_(RID_1)_RA" = 1
 r_102: + "x_Task_(TaskID_2)_A_Resource_(RID_3)_RB"
 + "x_Task_(TaskID_2)_A_Resource_(RID_2)_RA"
 + "x_Task_(TaskID_2)_A_Resource_(RID_1)_RA" = 1
 r_103: + "x_Task_(TaskID_1)_A_Resource_(RID_3)_RB"
 + "x_Task_(TaskID_1)_A_Resource_(RID_2)_RA"
 + "x_Task_(TaskID_1)_A_Resource_(RID_1)_RA" = 1
 r_104: + 3 "x_Task_(TaskID_102)_(B_3)_Resource_(RID_3)_RB"
 + "s_Task_(TaskID_102)_(B_3)" - "C_max" <= 0
 r_105: + 3 "x_Task_(TaskID_101)_(B_3)_Resource_(RID_3)_RB"
 + "s_Task_(TaskID_101)_(B_3)" - "C_max" <= 0
 r_106: + "x_Task_(TaskID_4)_A_Resource_(RID_2)_RA"
 + "s_Task_(TaskID_4)_A" - "C_max" <= 0
 r_107: + "x_Task_(TaskID_4)_A_Resource_(RID_1)_RA"
 + "s_Task_(TaskID_4)_A" - "C_max" <= 0
 r_108: + "x_Task_(TaskID_3)_A_Resource_(RID_2)_RA"
 + "s_Task_(TaskID_3)_A" - "C_max" <= 0
 r_109: + "x_Task_(TaskID_3)_A_Resource_(RID_1)_RA"
 + "s_Task_(TaskID_3)_A" - "C_max" <= 0
 r_110: + "x_Task_(TaskID_2)_A_Resource_(RID_2)_RA"
 + "s_Task_(TaskID_2)_A" - "C_max" <= 0
 r_111: + "x_Task_(TaskID_2)_A_Resource_(RID_1)_RA"
 + "s_Task_(TaskID_2)_A" - "C_max" <= 0
 r_112: + "x_Task_(TaskID_1)_A_Resource_(RID_2)_RA"
 + "s_Task_(TaskID_1)_A" - "C_max" <= 0
 r_113: + "x_Task_(TaskID_1)_A_Resource_(RID_1)_RA"
 + "s_Task_(TaskID_1)_A" - "C_max" <= 0

Bounds
 "C_max" free
 "Theta_Task_(TaskID_1)_A_Task_(TaskID_1)_A" free
 "Theta_Task_(TaskID_1)_A_Task_(TaskID_2)_A" free
 0 <= "Theta_Task_(TaskID_1)_A_Task_(TaskID_3)_A" <= 1
 0 <= "Theta_Task_(TaskID_1)_A_Task_(TaskID_4)_A" <= 1
 0 <= "Theta_Task_(TaskID_101)_(B_3)_Task_(TaskID_1)_A" <= 1
 "Theta_Task_(TaskID_101)_(B_3)_Task_(TaskID_101)_(B_3)" free
 "Theta_Task_(TaskID_101)_(B_3)_Task_(TaskID_102)_(B_3)" free
 0 <= "Theta_Task_(TaskID_101)_(B_3)_Task_(TaskID_3)_A" <= 1
 0 <= "Theta_Task_(TaskID_101)_(B_3)_Task_(TaskID_4)_A" <= 1
 0 <= "Theta_Task_(TaskID_102)_(B_3)_Task_(TaskID_1)_A" <= 1
 0 <= "Theta_Task_(TaskID_102)_(B_3)_Task_(TaskID_101)_(B_3)" <= 1
 "Theta_Task_(TaskID_102)_(B_3)_Task_(TaskID_102)_(B_3)" free
 0 <= "Theta_Task_(TaskID_102)_(B_3)_Task_(TaskID_2)_A" <= 1
 0 <= "Theta_Task_(TaskID_102)_(B_3)_Task_(TaskID_3)_A" <= 1
 0 <= "Theta_Task_(TaskID_102)_(B_3)_Task_(TaskID_4)_A" <= 1
 0 <= "Theta_Task_(TaskID_2)_A_Task_(TaskID_1)_A" <= 1
 0 <= "Theta_Task_(TaskID_2)_A_Task_(TaskID_101)_(B_3)" <= 1
 "Theta_Task_(TaskID_2)_A_Task_(TaskID_2)_A" free
 0 <= "Theta_Task_(TaskID_2)_A_Task_(TaskID_3)_A" <= 1
 0 <= "Theta_Task_(TaskID_2)_A_Task_(TaskID_4)_A" <= 1
 0 <= "Theta_Task_(TaskID_3)_A_Task_(TaskID_1)_A" <= 1
 0 <= "Theta_Task_(TaskID_3)_A_Task_(TaskID_101)_(B_3)" <= 1
 0 <= "Theta_Task_(TaskID_3)_A_Task_(TaskID_102)_(B_3)" <= 1
 0 <= "Theta_Task_(TaskID_3)_A_Task_(TaskID_2)_A" <= 1
 "Theta_Task_(TaskID_3)_A_Task_(TaskID_3)_A" free
 0 <= "Theta_Task_(TaskID_3)_A_Task_(TaskID_4)_A" <= 1
 0 <= "Theta_Task_(TaskID_4)_A_Task_(TaskID_1)_A" <= 1
 0 <= "Theta_Task_(TaskID_4)_A_Task_(TaskID_101)_(B_3)" <= 1
 0 <= "Theta_Task_(TaskID_4)_A_Task_(TaskID_102)_(B_3)" <= 1
 0 <= "Theta_Task_(TaskID_4)_A_Task_(TaskID_2)_A" <= 1
 0 <= "Theta_Task_(TaskID_4)_A_Task_(TaskID_3)_A" <= 1
 "Theta_Task_(TaskID_4)_A_Task_(TaskID_4)_A" free
 0 <= "x_Task_(TaskID_1)_A_Resource_(RID_1)_RA" <= 1
 0 <= "x_Task_(TaskID_1)_A_Resource_(RID_2)_RA" <= 1
 "x_Task_(TaskID_1)_A_Resource_(RID_3)_RB" free
 "x_Task_(TaskID_101)_(B_3)_Resource_(RID_1)_RA" free
 "x_Task_(TaskID_101)_(B_3)_Resource_(RID_2)_RA" free
 0 <= "x_Task_(TaskID_101)_(B_3)_Resource_(RID_3)_RB" <= 1
 "x_Task_(TaskID_102)_(B_3)_Resource_(RID_1)_RA" free
 "x_Task_(TaskID_102)_(B_3)_Resource_(RID_2)_RA" free
 0 <= "x_Task_(TaskID_102)_(B_3)_Resource_(RID_3)_RB" <= 1
 0 <= "x_Task_(TaskID_2)_A_Resource_(RID_1)_RA" <= 1
 0 <= "x_Task_(TaskID_2)_A_Resource_(RID_2)_RA" <= 1
 "x_Task_(TaskID_2)_A_Resource_(RID_3)_RB" free
 0 <= "x_Task_(TaskID_3)_A_Resource_(RID_1)_RA" <= 1
 0 <= "x_Task_(TaskID_3)_A_Resource_(RID_2)_RA" <= 1
 "x_Task_(TaskID_3)_A_Resource_(RID_3)_RB" free
 0 <= "x_Task_(TaskID_4)_A_Resource_(RID_1)_RA" <= 1
 0 <= "x_Task_(TaskID_4)_A_Resource_(RID_2)_RA" <= 1
 "x_Task_(TaskID_4)_A_Resource_(RID_3)_RB" free

Generals
 "Theta_Task_(TaskID_1)_A_Task_(TaskID_3)_A"
 "Theta_Task_(TaskID_1)_A_Task_(TaskID_4)_A"
 "Theta_Task_(TaskID_101)_(B_3)_Task_(TaskID_1)_A"
 "Theta_Task_(TaskID_101)_(B_3)_Task_(TaskID_3)_A"
 "Theta_Task_(TaskID_101)_(B_3)_Task_(TaskID_4)_A"
 "Theta_Task_(TaskID_102)_(B_3)_Task_(TaskID_1)_A"
 "Theta_Task_(TaskID_102)_(B_3)_Task_(TaskID_101)_(B_3)"
 "Theta_Task_(TaskID_102)_(B_3)_Task_(TaskID_2)_A"
 "Theta_Task_(TaskID_102)_(B_3)_Task_(TaskID_3)_A"
 "Theta_Task_(TaskID_102)_(B_3)_Task_(TaskID_4)_A"
 "Theta_Task_(TaskID_2)_A_Task_(TaskID_1)_A"
 "Theta_Task_(TaskID_2)_A_Task_(TaskID_101)_(B_3)"
 "Theta_Task_(TaskID_2)_A_Task_(TaskID_3)_A"
 "Theta_Task_(TaskID_2)_A_Task_(TaskID_4)_A"
 "Theta_Task_(TaskID_3)_A_Task_(TaskID_1)_A"
 "Theta_Task_(TaskID_3)_A_Task_(TaskID_101)_(B_3)"
 "Theta_Task_(TaskID_3)_A_Task_(TaskID_102)_(B_3)"
 "Theta_Task_(TaskID_3)_A_Task_(TaskID_2)_A"
 "Theta_Task_(TaskID_3)_A_Task_(TaskID_4)_A"
 "Theta_Task_(TaskID_4)_A_Task_(TaskID_1)_A"
 "Theta_Task_(TaskID_4)_A_Task_(TaskID_101)_(B_3)"
 "Theta_Task_(TaskID_4)_A_Task_(TaskID_102)_(B_3)"
 "Theta_Task_(TaskID_4)_A_Task_(TaskID_2)_A"
 "Theta_Task_(TaskID_4)_A_Task_(TaskID_3)_A"
 "s_Task_(TaskID_1)_A"
 "s_Task_(TaskID_101)_(B_3)"
 "s_Task_(TaskID_102)_(B_3)"
 "s_Task_(TaskID_2)_A"
 "s_Task_(TaskID_3)_A"
 "s_Task_(TaskID_4)_A"
 "x_Task_(TaskID_1)_A_Resource_(RID_1)_RA"
 "x_Task_(TaskID_1)_A_Resource_(RID_2)_RA"
 "x_Task_(TaskID_101)_(B_3)_Resource_(RID_3)_RB"
 "x_Task_(TaskID_102)_(B_3)_Resource_(RID_3)_RB"
 "x_Task_(TaskID_2)_A_Resource_(RID_1)_RA"
 "x_Task_(TaskID_2)_A_Resource_(RID_2)_RA"
 "x_Task_(TaskID_3)_A_Resource_(RID_1)_RA"
 "x_Task_(TaskID_3)_A_Resource_(RID_2)_RA"
 "x_Task_(TaskID_4)_A_Resource_(RID_1)_RA"
 "x_Task_(TaskID_4)_A_Resource_(RID_2)_RA"

End
