run:
	swipl bulletpl7.pl -- 'bulletpl/ex1/rand.pl' \
	 'bulletpl/ex1/cross1.pl' \
	 'bulletpl/ex1/cross2.pl' \
	 'bulletpl/ex1/dirSeq8.pl' \
	 'bulletpl/ex1/dirSeq3way.pl' \
	 'bulletpl/ex1/dirSeqRotate.pl' \
	 'bulletpl/ex1/spdSeq3.pl' \
	 'bulletpl/ex1/f5.pl' \
	 'bulletpl/ex1/f4.pl' \
	 'bulletpl/ex1/f3.pl' \
	 'bulletpl/ex1/f2.pl' \
	 'bulletpl/ex1/bulletRef.pl'
	swipl bulletpl7.pl -- 'bulletpl/[ESP_RADE]_round_5_alice_clone.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[Original]_backfire.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[Original]_guruguru.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[Original]_housya.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[Original]_progear_cheap_fake.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[Original]_zako_atack.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[Bulletsmorph]_aba_4.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[Daiouzyou]_round_4_boss_1.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[Daiouzyou]_round_4_boss_2.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[Daiouzyou]_round_6_boss_5.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[Dodonpachi]_kitiku_2.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[ESP_RADE]_round_5_boss_gara_2.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[Garegga]_black_heart_mk2_winder.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[Progear]_round_5_boss_last_round_wave.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[Progear]_round_5_middle_boss_rockets.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[Progear]_round_9_boss.xml' #ok1
	swipl bulletpl7.pl -- 'bulletpl/[SilverGun]_4D_boss_PENTA.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[XEVIOUS]_garu_zakato.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/zakato.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[Original]_kagome.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[Original]_knight_1.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[tenmado]_5_boss_1.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[Bulletsmorph]_aba_1.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[Bulletsmorph]_aba_3.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[Daiouzyou]_hibachi_1.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[Daiouzyou]_hibachi_2.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Daiouzyou]_hibachi_4.xml'
	swipl bulletpl7.pl -- 'bulletpl/[ESP_RADE]_round_5_boss_gara_4.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Guwange]_round_4_boss_eye_ball.xml' # ok
	swipl bulletpl7.pl -- 'bulletpl/[Ketui_LT]_3boss_kunekune.xml' #ok ayashi
	swipl bulletpl7.pl -- 'bulletpl/[MDA]_acc_n_dec.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[MDA]_double_w.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[MDA]_gnnnyari.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[MDA]_mojya.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[Noiz2sa]_bit.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[Original]_dokkaan.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[Original]_hasami.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[Original]_knight_3.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[Original]_oogi_hutatsu.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[Original]_sakuretudan.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[Original]_star_in_the_sky.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[Original]_stone6.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[Original]_tsunami.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[OtakuTwo]_dis_bee_3.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[OtakuTwo]_self-1020.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[Progear]_round_1_boss_grow_bullets.xml' #ok ayashi
	swipl bulletpl7.pl -- 'bulletpl/[Progear]_round_2_boss_struggling.xml' #ok ayashi
	swipl bulletpl7.pl -- 'bulletpl/[Psyvariar]_4-D_boss_MZIQ.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[Psyvariar]_X-A_boss_opening.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[Psyvariar]_X-B_colony_shape_satellite.xml' #ok
	swipl bulletpl7.pl -- 'bulletpl/[Strikers1999]_hanabi.xml' #ok
	make run2
run2: bulletref_param ayashii_ok fspd_ok pspd_ok pdir_ok param_ok_evalSpdDir param_ok prolog_syntax_ok pdir_kieru_ok
bulletref_param_ok:
	swipl bulletpl7.pl -- 'bulletpl/[Bulletsmorph]_aba_2.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Bulletsmorph]_aba_6.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Bulletsmorph]_convergent.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Bulletsmorph]_double_seduction.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Daiouzyou]_round_3_boss.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Daiouzyou]_round_4_boss.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Daiouzyou]_round_5_boss_2.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Daiouzyou]_round_6_boss_1.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Daiouzyou]_round_6_boss_3.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Daiouzyou]_round_6_boss_4.xml'
	swipl bulletpl7.pl -- 'bulletpl/[ESP_RADE]_round_5_boss_gara_3.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Guwange]_round_3_boss_fast_3way.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Original]_accusation.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Original]_balloon_bomb.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Original]_btb_1.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Original]_btb_2.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Original]_btb_3.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Original]_btb_4.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Original]_btb_5.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Original]_btb_6.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Original]_censored.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Original]_chimera.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Original]_ellipse_bomb.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Original]_fujin_ranbu_fake.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Original]_fujin_ranbu_true.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Original]_hajike.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Original]_knight_2.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Original]_knight_4.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Original]_light_lv10.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Original]_light_lv25.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Original]_light_max.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Original]_pan.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Original]_water_lv10.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Original]_water_max.xml'
	swipl bulletpl7.pl -- 'bulletpl/[OtakuTwo]_self-2010.xml'
	swipl bulletpl7.pl -- 'bulletpl/[OtakuTwo]_self-2011.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Progear]_round_3_boss_wave_bullets.xml'
	swipl bulletpl7.pl -- 'bulletpl/[tenmado]_3_boss_2.xml'
	swipl bulletpl7.pl -- 'bulletpl/[xsoldier]_8_boss_main.xml'
ayashii_ok:
	swipl bulletpl7.pl -- 'bulletpl/[G_DARIUS]_homing_laser.xml' #ok ayashi
	swipl bulletpl7.pl -- 'bulletpl/[1943]_rolling_fire.xml' #ok1
fspd_ok:
	swipl bulletpl7.pl -- 'bulletpl/[ChaosSeed]_big_monkey_boss.xml'
pspd_ok:
	swipl bulletpl7.pl -- 'bulletpl/[Daiouzyou]_hibachi_3.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Daiouzyou]_round_6_boss_2.xml'
	swipl bulletpl7.pl -- 'bulletpl/[MDA]_circular_sun.xml'
pdir_ok:
	swipl bulletpl7.pl -- 'bulletpl/[OtakuTwo]_dis_bee_1.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Dodonpachi]_hibachi.xml'
	swipl bulletpl7.pl -- 'bulletpl/[ESP_RADE]_round_5_boss_gara_1_a.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Original]_gyakuhunsya.xml'
	swipl bulletpl7.pl -- 'bulletpl/[OtakuTwo]_circle_fireworks.xml'
	swipl bulletpl7.pl -- 'bulletpl/[OtakuTwo]_circle_fireworks2.xml'
	swipl bulletpl7.pl -- 'bulletpl/[OtakuTwo]_circle_roll.xml'
	swipl bulletpl7.pl -- 'bulletpl/[OtakuTwo]_roll_misago.xml'
	swipl bulletpl7.pl -- 'bulletpl/[OtakuTwo]_self-0062.xml'
	swipl bulletpl7.pl -- 'bulletpl/[OtakuTwo]_self-0063.xml'
param_ok_evalSpdDir:
	swipl bulletpl7.pl -- 'bulletpl/[MDA]_circular.xml'
	swipl bulletpl7.pl -- 'bulletpl/[MDA]_circular_model.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Original]_shooting_star.xml'
	swipl bulletpl7.pl -- 'bulletpl/[OtakuTwo]_circle_trap.xml'
param_ok:
	swipl bulletpl7.pl -- 'bulletpl/[MDA]_mossari.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Daiouzyou]_round_3_boss_last.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Dodonpachi]_kitiku_1.xml'
	swipl bulletpl7.pl -- 'bulletpl/[ESP_RADE]_round_5_boss_ares_2.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Guwange]_round_2_boss_circle_fire.xml'
	swipl bulletpl7.pl -- 'bulletpl/[MDA]_14b_2-3w.xml'
	swipl bulletpl7.pl -- 'bulletpl/[MDA]_2f.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Original]_gurutyo.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Original]_two_cross.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Original]_uneri.xml'
	swipl bulletpl7.pl -- 'bulletpl/[OtakuTwo]_slow_move.xml'
	swipl bulletpl7.pl -- 'bulletpl/[MDA]_fukuro.xml'
	swipl bulletpl7.pl -- 'bulletpl/[OtakuTwo]_restriction_stasis.xml'

prolog_syntax_ok:
	swipl bulletpl7.pl -- 'bulletpl/[ESP_RADE]_round_5_boss_gara_1_b.xml'
	swipl bulletpl7.pl -- 'bulletpl/[ESP_RADE]_round_5_boss_kakusi_hakkyou.xml'
	swipl bulletpl7.pl -- 'bulletpl/[MDA]_wind_cl.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Noiz2sa]_88way.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Original]_circle.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Original]_evil_eye.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Daiouzyou]_round_4_boss_5.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Daiouzyou]_round_5_boss_1.xml'
	swipl bulletpl7.pl -- 'bulletpl/[ESP_RADE]_round_123_boss_izuna_fan.xml'
	swipl bulletpl7.pl -- 'bulletpl/[ESP_RADE]_round_123_boss_izuna_hakkyou.xml'
	swipl bulletpl7.pl -- 'bulletpl/[ESP_RADE]_round_123_boss_pelaboy_hakkyou.xml'
	swipl bulletpl7.pl -- 'bulletpl/[ESP_RADE]_round_123_boss_satoru_5way.xml'
	swipl bulletpl7.pl -- 'bulletpl/[G-Wange]_round_trip_bit.xml'

pdir_kieru_ok:
	swipl bulletpl7.pl -- 'bulletpl/[OtakuTwo]_dis_bee_2.xml'
	swipl bulletpl7.pl -- 'bulletpl/[OtakuTwo]_self-0034.xml'
	swipl bulletpl7.pl -- 'bulletpl/[OtakuTwo]_self-0071.xml'
	swipl bulletpl7.pl -- 'bulletpl/[OtakuTwo]_self-2020.xml'
accel_error:
	swipl bulletpl7.pl -- 'bulletpl/[Bulletsmorph]_fallen_string.xml'
	swipl bulletpl7.pl -- 'bulletpl/[ESP_RADE]_round_5_boss_gara_5.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Ketui_LT]_1boss_bit.xml'
	swipl bulletpl7.pl -- 'bulletpl/[MDA]_75l-42.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Original]_cont_circle.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Original]_kunekune.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Original]_stop_and_run.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Original]_water_lv25.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Original]_yokokasoku.xml'
	swipl bulletpl7.pl -- 'bulletpl/[OtakuTwo]_accel_jump.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Progear]_round_3_boss_back_burst.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Progear]_round_4_boss_fast_rocket.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Progear]_round_6_boss_parabola_shot.xml'
	swipl bulletpl7.pl -- 'bulletpl/[OtakuTwo]_self-0081.xml'
errors:
	swipl bulletpl7.pl -- 'bulletpl/[Daiouzyou]_round_1_boss.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Daiouzyou]_round_1_boss_hakkyou.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Dodonpachi]_kitiku_3.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Dodonpachi]_kitiku_5.xml'
	swipl bulletpl7.pl -- 'bulletpl/[DragonBlaze]_nebyurosu_2.xml'
	swipl bulletpl7.pl -- 'bulletpl/[G-Wange]_roll_gara.xml'
	swipl bulletpl7.pl -- 'bulletpl/[GigaWing2]_akurimi.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Noiz2sa]_rollbar.xml'
	swipl bulletpl7.pl -- 'bulletpl/[OtakuTwo]_dis_bee_hakkyou.xml'
	swipl bulletpl7.pl -- 'bulletpl/[OtakuTwo]_self-1021.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Progear]_round_10_boss_before_final.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Psyvariar]_X-A_boss_winder.xml'
	swipl bulletpl7.pl -- 'bulletpl/[STORM_CALIBAR]_last_boss_double_roll_bullets.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Bulletsmorph]_kunekune_plus_homing.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Daiouzyou]_round_3_boss_2.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Original]_hirahira.xml'
	swipl bulletpl7.pl -- 'bulletpl/[OtakuTwo]_self-0012.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Daiouzyou]_hibachi_image.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Daiouzyou]_hibachi_maybe.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Daiouzyou]_round_4_boss_4.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Ketui_LT]_3boss_roll_and_aim.xml'
	swipl bulletpl7.pl -- 'bulletpl/[Original]_wana.xml'

run_omoi:
	swipl bulletpl7.pl -- 'bulletpl/[Bulletsmorph]_aba_7.xml' #ok omo
	swipl bulletpl7.pl -- 'bulletpl/[Ketui_LT]_2boss_winder_crash.xml' # omoi
	swipl bulletpl7.pl -- 'bulletpl/1943_rolling_fire.xml' #ok1
	swipl bulletpl7.pl -- 'bulletpl/[Original]_air_elemental.xml' # omo
	swipl bulletpl7.pl -- 'bulletpl/[Bulletsmorph]_aba_5.xml' #omo
	swipl bulletpl7.pl -- 'bulletpl/[MDA]_10flower_2.xml' #omo
	swipl bulletpl7.pl -- 'bulletpl/[Original]_extinction.xml' #omo
	swipl bulletpl7.pl -- 'bulletpl/[Original]_optic_seeker.xml' #omo
	swipl bulletpl7.pl -- 'bulletpl/[Original]_time_twist.xml' #omo
	swipl bulletpl7.pl -- 'bulletpl/[tenmado]_5_boss_3.xml' #omo
	swipl bulletpl7.pl -- 'bulletpl/[Noiz2sa]_5_players.xml' %omo
	swipl bulletpl7.pl -- 'bulletpl/[Original]_entangled_space.xml' #omo
	swipl bulletpl7.pl -- 'bulletpl/[Original]_kedama.xml' #omo
	swipl bulletpl7.pl -- 'bulletpl/[Original]_kujira.xml' #omo
	swipl bulletpl7.pl -- 'bulletpl/[XII_STAG]_3b.xml' #omo


all:
	swipl load.pl
test:
	swipl syntax_test.pl
diff:
	meld examples bulletpl
diff2:
	code --diff examples bulletpl
