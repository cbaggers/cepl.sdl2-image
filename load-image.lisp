(in-package #:cepl.sdl2-image)

(defun load-image-to-c-array (filename)
  (let ((surface (sdl2-image:load-image filename)))
    (unwind-protect
	 (let* ((sdl-format (sdl2:surface-format-format surface))
		(pixel-format (sdl2-surface-format->pixel-format sdl-format))
		(element-type (pixel-format->lisp-type pixel-format))
		(width (sdl2:surface-width surface))
		(height (sdl2:surface-height surface))
		(ptr (sdl2:surface-pixels surface)))
	   (make-c-array-from-pointer
	    (list width height) element-type ptr))
      (sdl2::sdl-cancel-collect surface)
      (cffi:foreign-free (autowrap:ptr surface))
      (autowrap:invalidate surface))))

(defun load-image-to-texture (filename &optional (image-format :rgba8))
  (let ((surface (sdl2-image:load-image filename)))
    (unwind-protect
	 (let* ((sdl-format (sdl2:surface-format-format surface))
		(pixel-format (sdl2-surface-format->pixel-format sdl-format))
		(element-type (pixel-format->lisp-type pixel-format))
		(width (sdl2:surface-width surface))
		(height (sdl2:surface-height surface))
		(ptr (sdl2:surface-pixels surface)))
	   (let ((arr (make-c-array-from-pointer
		       (list width height) element-type ptr)))
	     (make-texture arr :element-type image-format
			   :pixel-format pixel-format)))
      (sdl2:free-surface surface))))

(defun sdl2-surface-format->pixel-format (surface-format)
  (ecase surface-format
    (:bgr24 (cepl.types::make-pixel-format
	     :components :bgr
	     :type :uint8
	     :normalise t
	     :sizes nil
	     :reversed nil
	     :comp-length 3))
    (:bgr565 (cepl.types::make-pixel-format
	      :components :bgr
	      :type :ushort
	      :normalise t
	      :sizes '(5 6 5)
	      :reversed nil
	      :comp-length 3))
    (:bgr888 (cepl.types::make-pixel-format
		:components :bgr
		:type :uint8
		:normalise t
		:sizes nil
		:reversed nil
		:comp-length 3))
    (:bgra4444 (cepl.types::make-pixel-format
		:components :bgra
		:type :ushort
		:normalise t
		:sizes '(4 4 4 4)
		:reversed nil
		:comp-length 4))
    (:bgra5551 (cepl.types::make-pixel-format
		:components :bgra
		:type :ushort
		:normalise t
		:sizes '(5 5 5 1)
		:reversed nil
		:comp-length 4))
    (:bgra8888 (cepl.types::make-pixel-format
		:components :bgra
		:type :uint8
		:normalise t
		:sizes '(8 8 8 8)
		:reversed nil
		:comp-length 4))

    (:argb1555 (cepl.types::make-pixel-format
		:components :bgra
		:type :ushort
		:normalise t
		:sizes '(5 5 5 1)
		:reversed t
		:comp-length 4))
    (:argb4444 (cepl.types::make-pixel-format
		:components :bgra
		:type :ushort
		:normalise t
		:sizes '(4 4 4 4)
		:reversed t
		:comp-length 4))
    (:argb8888 (cepl.types::make-pixel-format
		:components :bgra
		:type :uint8
		:normalise t
		:sizes '(8 8 8 8)
		:reversed nil
		:comp-length 4))

    (:rgb24 (cepl.types::make-pixel-format
	     :components :rgb
	     :type :uint8
	     :normalise t
	     :sizes nil
	     :reversed nil
	     :comp-length 3))
    (:rgb332 (cepl.types::make-pixel-format
	      :components :rgb
	      :type :uint8
	      :normalise t
	      :sizes '(3 3 2)
	      :reversed nil
	      :comp-length 3))
    (:rgb565 (cepl.types::make-pixel-format
	      :components :rgb
	      :type :ushort
	      :normalise t
	      :sizes '(5 6 5)
	      :reversed nil
	      :comp-length 3))
    (:rgb888 (cepl.types::make-pixel-format
	      :components :rgb
	      :type :uint8
	      :normalise t
	      :sizes 'nil
	      :reversed nil
	      :comp-length 3))
    (:rgba4444 (cepl.types::make-pixel-format
		:components :rgba
		:type :ushort
		:normalise t
		:sizes '(4 4 4 4)
		:reversed nil
		:comp-length 4))
    (:rgba5551 (cepl.types::make-pixel-format
		:components :rgba
		:type :ushort
		:normalise t
		:sizes '(5 5 5 1)
		:reversed nil
		:comp-length 4))
    (:rgba8888 (cepl.types::make-pixel-format
		:components :rgba
		:type :uint8
		:normalise t
		:sizes '(8 8 8 8)
		:reversed nil
		:comp-length 4))


    (:abgr1555 (cepl.types::make-pixel-format
		:components :rgba
		:type :ushort
		:normalise t
		:sizes '(5 5 5 1)
		:reversed t
		:comp-length 4))
    (:abgr4444 (cepl.types::make-pixel-format
		:components :rgba
		:type :ushort
		:normalise t
		:sizes '(4 4 4 4)
		:reversed t
		:comp-length 4))
    (:abgr8888 (cepl.types::make-pixel-format
		:components :rgba
		:type :uint8
		:normalise t
		:sizes '(8 8 8 8)
		:reversed t
		:comp-length 4))))

;; (:index1lsb)
;; (:index1msb)
;; (:index4lsb)
;; (:index4msb)
;; (:index8)

;; (:argb2101010)

;; (:bgr555)
;; (:bgrx8888)

;; (:rgb444)
;; (:rgb555)
;; (:rgbx8888)

;; :yv12
;; planar mode: Y + V + U (3 planes)

;; :iyuv
;; planar mode: Y + U + V (3 planes)

;; :yuy2
;; packed mode: Y0+U0+Y1+V0 (1 plane)

;; :uyvy
;; packed mode: U0+Y0+V0+Y1 (1 plane)
