#lang racket

(provide
 screen%)

; An abstraction for a single in game interface
;   inventory, menus, game screen, etc.
(define screen%
  (class object%
    ; Called when the user pressed a key
    ; Should return the screen to use for the next tick (often 'this')
    (define/public (update key-event)
      (error 'screen% "override this method"))
    
    ; Called when the GUI needs to draw this screen
    ; Passed an ascii-canvas to draw to
    (define/public (draw canvas)
      (error 'screen% "override this method"))
    
    (super-new)))