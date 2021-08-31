#lang racket

#|
Copyright 2020 Jos'h Fuller

This file is part of Zodetrip.

Zodetrip is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Zodetrip is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Zodetrip.  If not, see <https://www.gnu.org/licenses/>.
|#

(provide ROOMS)

(define ROOMS
  '((default
      (width map 3)
      (height map 3)
      (tiles map (-1 -1 -1  -1 -1 -1  -1 -1 -1))
      (event enter
	     ((tell "SOMETHING BAD HAPPENED."
		   "PLEASE REPORT THIS BUG!" ""
		   "THNX 1.0E6!"))))

    (Title (theme music title)
           (width map 6)
           (height map 5)
           (tiles map (48 49 50 51 52 53
			  64 65 66 67 68 69
			  15 15 15 15 15 15
			  15 15 54 55 15 15
			  -1 -1 -1 -1 -1 -1
			  ))
	   (trap map ((0 4)
		      (tell "TRY GOING THE OTHER WAY..." "" ""
			    "(SPACE) or (RETURN) TO GO ON.")))
	   (trap map ((5 4)
		      (tell "WELCOME TO ZODE-TRIP!" "" ""
			    "PRESS (ENTER) TO CONTINUE.")
		      (tell "THE YEAR IS 2081."  ""
			    "THE WORLD IS DIVIDED INTO 23"
			    "ZONE NODES (zodes)...")
		      (tell "LINKED BY touchstones. THE"
			    "TOUCHSTONES MANAGE LOCAL"
			    "COMPUTE RESOURCES AND ARTIFICIAL"
			    "REALITY. HOWEVER...")
		      (tell "ONE DAY IN ZODE chorar, A YOUNG"
			    "BOY REALIZES THAT SOMETHING IS"
			    "WRONG..."
			    "HE SETS OUT TO PUT IT RIGHT!")
		      (tell "USE ARROW KEYS TO MOVE."
			    "TOUCH SOMETHING (OR SOMEONE) TO"
			    "INTERACT. WALK INTO DOORS TO"
			    "ENTER ROOMS...")
		      (jump Hometown (6 2) right))))
                        
    (Hometown
     (theme music hometown)
     (width map 16)
     (height map 9)
     (trap map ((7 6)
		(sfx door)
		(jump Home (5 6) left)))
     (trap map
	   ((6 6)
	    (is? front-window (end))
	    (tell "WAIT A MINUTE..." ""
		  "DON'T WE HAVE THREE WINDOWS"
		  "IN FRONT OF THE HOUSE?")
	    (set! front-window)
	    ))
     (trap map ((2 4) (tell "THIS IS SENHORA'S SHRINE.")))
     (trap map ((11 4) (tell "I GUESS JASMIN ISN'T HOME NOW.")))
     (trap map ((12 0) (tell "THIS IS THE WAY TO THE GARAGE.")))
     (trap map ((5 8) (tell "THIS IS THE WAY TO THE GREENHOUSE.")))
     (tiles
      map
    (192 193 193 193 193 193 193 193 193 193 193 225 -1 226 194 195
    208 245 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 242 -1 211 208 -1 -1 -1
    245 -1 -1 -1 -1 202 203 203 203 204 -1 211 208 218 219 220 -1 -1
    -1 245 -1 212 216 216 216 217 -1 211 208 234 235 236 -1 -1 -1 -1
    -1 228 216 230 232 233 -1 211 208 -1 -1 -1 -1 196 197 197 197 198
    245 -1 -1 -1 -1 211 208 245 -1 -1 -1 228 229 230 229 233 -1 -1 -1
    245 -1 211 208 -1 -1 245 -1 -1 -1 -1 -1 -1 -1 -1 245 -1 -1 211 224
    193 193 193 225 -1 226 193 193 193 193 193 193 193 193 227)))
    
    (Home
     (theme music home)
     (width map 10)
     (height map 8)
     (trap map ((7 1) (is? home-exit-1 (end)) (tell "I'M NOT SLEEPY!")))
     (trap map ((8 1) (is? home-exit-1 (end)) (tell "COMFY PILLOW..." "" "" "BUT, I'M STILL NOT SLEEPY!")))
     (trap map ((7 5) (tell "NOT LUNCHTIME YET...")))
     (trap map ((1 6) (is? home-exit-1 (end))
		(tell "MOM LEFT A NOTE..." "" "SHE'S AT THE GREENHOUSE TODAY...")))
     (trap map ((5 7)
		(is? home-exit-1
		     (sfx door)
		     (jump Hometown (7 7) left))
		(tell "I'LL GO OUT NOW!")
		(set! home-exit-1)
		(sfx door)
		(jump Hometown (7 7) left)))
     (tiles map
      (-1 -1 -1 -1 -1 128 134 130 131 135 128 130 130 129 130 144 -1
          148 149 167 160 -1 -1 -1 -1 167 -1 -1 -1 167 160 162 161 -1
          -1 151 177 -1 182 144 151 134 177 -1 182 183 -1 -1 -1 167
          160 -1 -1 -1 -1 -1 -1 165 -1 167 160 164 -1 -1 -1 -1 -1 -1
          -1 167 176 130 134 130 177 -1 182 130 134 183)))
    ))
