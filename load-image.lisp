(in-package #:cepl.sdl2-image)

(defun load-image-to-c-array (filename)
  (let ((surface (sdl2-image:load-image filename)))
    (unwind-protect
	 (let* ((sdl-format (sdl2:surface-format-format surface))
		(pixel-format (sdl2-surface-format->pixel-format sdl-format))
		(element-type (pixel-format->lisp-type pixel-format))
		(width (sdl2:surface-width surface))
		(height (sdl2:surface-height surface))
		(pixel-ptr (sdl2:surface-pixels surface))
		(byte-size (* (sdl2:surface-pitch surface)
			      (sdl2:surface-height surface)))
		(data-ptr (cffi:foreign-alloc :uint8 :count byte-size)))
	   (cepl.types::%memcpy data-ptr pixel-ptr byte-size)
	   (make-c-array-from-pointer (list width height) element-type data-ptr))
      (sdl2:free-surface surface))))

(defun load-image-to-texture (filename &optional (image-format :rgba8) mipmap
					 generate-mipmaps)
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
			   :pixel-format pixel-format :mipmap mipmap
			   :generate-mipmaps generate-mipmaps)))
      (sdl2:free-surface surface))))

(defun sdl2-surface-format->pixel-format (surface-format)
  (ecase surface-format
    (:bgr24 (cepl.types::make-pixel-format
	     :components :bgr
	     :type :uint8
	     :normalize t
	     :sizes nil
	     :reversed nil
	     :comp-length 3))
    (:bgr565 (cepl.types::make-pixel-format
	      :components :bgr
	      :type :ushort
	      :normalize t
	      :sizes '(5 6 5)
	      :reversed nil
	      :comp-length 3))
    (:bgr888 (cepl.types::make-pixel-format
		:components :bgr
		:type :uint8
		:normalize t
		:sizes nil
		:reversed nil
		:comp-length 3))
    (:bgra4444 (cepl.types::make-pixel-format
		:components :bgra
		:type :ushort
		:normalize t
		:sizes '(4 4 4 4)
		:reversed nil
		:comp-length 4))
    (:bgra5551 (cepl.types::make-pixel-format
		:components :bgra
		:type :ushort
		:normalize t
		:sizes '(5 5 5 1)
		:reversed nil
		:comp-length 4))
    (:bgra8888 (cepl.types::make-pixel-format
		:components :bgra
		:type :uint8
		:normalize t
		:sizes nil
		:reversed nil
		:comp-length 4))

    (:argb1555 (cepl.types::make-pixel-format
		:components :bgra
		:type :ushort
		:normalize t
		:sizes '(5 5 5 1)
		:reversed t
		:comp-length 4))
    (:argb4444 (cepl.types::make-pixel-format
		:components :bgra
		:type :ushort
		:normalize t
		:sizes '(4 4 4 4)
		:reversed t
		:comp-length 4))
    (:argb8888 (cepl.types::make-pixel-format
		:components :bgra
		:type :uint8
		:normalize t
		:sizes nil
		:reversed nil
		:comp-length 4))

    (:rgb24 (cepl.types::make-pixel-format
	     :components :rgb
	     :type :uint8
	     :normalize t
	     :sizes nil
	     :reversed nil
	     :comp-length 3))
    (:rgb332 (cepl.types::make-pixel-format
	      :components :rgb
	      :type :uint8
	      :normalize t
	      :sizes '(3 3 2)
	      :reversed nil
	      :comp-length 3))
    (:rgb565 (cepl.types::make-pixel-format
	      :components :rgb
	      :type :ushort
	      :normalize t
	      :sizes '(5 6 5)
	      :reversed nil
	      :comp-length 3))
    (:rgb888 (cepl.types::make-pixel-format
	      :components :rgb
	      :type :uint8
	      :normalize t
	      :sizes nil
	      :reversed nil
	      :comp-length 3))
    (:rgba4444 (cepl.types::make-pixel-format
		:components :rgba
		:type :ushort
		:normalize t
		:sizes '(4 4 4 4)
		:reversed nil
		:comp-length 4))
    (:rgba5551 (cepl.types::make-pixel-format
		:components :rgba
		:type :ushort
		:normalize t
		:sizes '(5 5 5 1)
		:reversed nil
		:comp-length 4))
    (:rgba8888 (cepl.types::make-pixel-format
		:components :rgba
		:type :uint8
		:normalize t
		:sizes nil
		:reversed nil
		:comp-length 4))


    (:abgr1555 (cepl.types::make-pixel-format
		:components :rgba
		:type :ushort
		:normalize t
		:sizes '(5 5 5 1)
		:reversed t
		:comp-length 4))
    (:abgr4444 (cepl.types::make-pixel-format
		:components :rgba
		:type :ushort
		:normalize t
		:sizes '(4 4 4 4)
		:reversed t
		:comp-length 4))
    (:abgr8888 (cepl.types::make-pixel-format
		:components :rgba
		:type :uint8
		:normalize t
		:sizes nil
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
