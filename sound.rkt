#lang racket/gui
(provide play-safe PMUSICS MUSICS PSOUNDS SOUNDS)
(require "stubs.rkt" "av.rkt" racket/runtime-path)

(define-runtime-path-list PMUSICS
  '("a/title.ogg"
    "a/home.ogg"
    "a/hometown-003.ogg"))
(define MUSICS (for/list [(k '(title home hometown))
                          (v PMUSICS)] (list k v)))
(define-runtime-path-list PSOUNDS
  '("a/drum-samples/snare.wav"
    "a/sfx_foley_clank_3.oga"
    "a/sfx_foley_door_close.oga"
    "a/sfx_foley_footstep_normal_3.oga"
    "a/sfx_foley_footstep_soft_1.oga"
    "a/sfx_foley_footstep_stone_1.oga"
    "a/sfx_magical_effects_portal_fail.oga"
    "a/sfx_magical_effects_portal_open.oga"
    "a/sfx_magical_effects_stone_key_fail.oga"
    "a/sfx_magical_effects_stone_key.oga"
    "a/sfx_magical_effects_unlock.oga"
    ))
;; Sound
(define (play-safe name)
  "( name --) Play sound if possible. But don't crash."

  (let [(sound (av SOUNDS name))]
    (when sound (stub-play-channel -1 sound 0))))

(define SOUNDS
  (for/list [(k '(snare clank door step step-soft step-stone
                       portal-fail portal key-fail key unlock))
             (v PSOUNDS)]
    (list k (stub-load-wav v))))