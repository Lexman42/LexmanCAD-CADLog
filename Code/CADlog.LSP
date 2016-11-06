(vl-load-com)

;; MAIN EXPORT FUNCTION
(defun C:LCAD_Export ( / ss file ename filePath pt1 pt2 *error* propertiesString)


  ;; DEFINE ERROR HANDLING
  (defun *error* ( msg / )
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg))
    );_if
    (exit)
    (princ)
  );_defun *error*
    
  (princ "\nSelect Point, Line, or, PolyLine Entities to Export:\n")

  ;; ONLY SELECT POLYLINES WHOSE 70 CODE IS 0 OR LINES (WHICH HAVE NO 70 CODE)
  (setq ss (ssget '((-4 . "<or")
                      (0 . "LWPOLYLINE")
                      (0 . "POLYLINE")
                      (0 . "POINT")
                      (0 . "LINE")
                    (-4 . "or>"))))

  (if ss
    (progn

      ;; DETERMINE THE EXPORT FILE PATH
      (if (setq filePath (getfiled "Backup to CSV file" "" "csv" 1))
        (progn
            
          ;; OPEN THE FILE
          (if (setq file (open filePath "w" ))
            (progn

              ;; PREPARE HEADERS
              (write-line "ENTITY TYPE,X Coordinate,Y Coordinate,Z Coordinate" file)
              
              ;; WRITE TO THE FILE
              (while (> (sslength ss) 0)

                ;; BUILD THE PROPERTIES STRING
                (setq propertiesString (buildPropertiesString (ssname ss 0)))                

                (cond

                  ((= "LINE" (cdr (assoc 0 (entget (ssname ss 0)))))
                    (progn
                      (write-line "LINE:" file)

                      ;; PROPERTIES STRING IF NECESSARY
                      (if propertiesString (write-line propertiesString file))
                      
                      (setq pt1 (cdr (assoc 10 (entget (ssname ss 0)))))
                      (write-line (strcat "  VERTEX:," (rtos (car pt1) 2 2) "," (rtos (cadr pt1) 2 2) "," (rtos (caddr pt1) 2 2)) file)

                      (setq pt2 (cdr (assoc 11 (entget (ssname ss 0)))))
                      (write-line (strcat "  VERTEX:," (rtos (car pt2) 2 2) "," (rtos (cadr pt2) 2 2) "," (rtos (caddr pt2) 2 2)) file)

                    );_progn
                  );_(= "LINE" (cdr (assoc 0 (entget (ssname ss 0)))))


                  ((= "POINT" (cdr (assoc 0 (entget (ssname ss 0)))))
                    (progn
                      (write-line "POINT:" file)
                      
                      ;; PROPERTIES STRING IF NECESSARY
                      (if propertiesString (write-line propertiesString file))
                      
                      (setq pt1 (cdr (assoc 10 (entget (ssname ss 0)))))
                      (write-line (strcat "  VERTEX:," (rtos (car pt1) 2 2) "," (rtos (cadr pt1) 2 2) "," (rtos (caddr pt1) 2 2)) file)

                    );_progn
                  );_(= "POINT" (cdr (assoc 0 (entget (ssname ss 0)))))


                  ((= "LWPOLYLINE" (cdr (assoc 0 (entget (ssname ss 0)))))
                    (progn

                      ;; SPECIFY OPENED OR CLOSED LWPOLYLINE
                      (if (= 1 (logand (cdr (assoc 70 (entget (ssname ss 0)))) 1))                        
                        (write-line "LWPOLYLINE (closed):" file)
                        (write-line "LWPOLYLINE (open): "  file)
                      );_if

                      ;; PROPERTIES STRING IF NECESSARY
                      (if propertiesString (write-line propertiesString file))

                      ;; FETCH THE ENTITY LIST
                      (setq elist (entget (ssname ss 0)))
                      (setq elev (cdr (assoc 38 elist)))

                      ;; PROCESS EACH POINT
                      (while (setq pt1 (cdr (assoc 10 elist)))
                        (write-line (strcat "  VERTEX:," (rtos (car pt1) 2 2) "," (rtos (cadr pt1) 2 2) "," (rtos elev 2 2))file)
                        (setq elist (vl-remove (assoc 10 elist) elist))
                      );_while

                      (write-line "END OF LWPOLYLINE" file)

                    );_progn
                  );_(= "LWPOLYLINE" (cdr (assoc 0 (entget (ssname ss 0)))))

                  ;; 2D POLYLINE
                  ((and
                     (= "POLYLINE" (cdr (assoc 0 (entget (ssname ss 0)))))
                     (= 0 (logand (cdr (assoc 70 (entget (ssname ss 0))))8))
                   );_and
                   
                    (progn

                      (setq ename (ssname ss 0))

                      ;; SPECIFY OPENED OR CLOSED LWPOLYLINE
                      (if (= 1 (logand (cdr (assoc 70 (entget ename))) 1))                        
                        (write-line "2DPOLYLINE (closed):" file)
                        (write-line "2DPOLYLINE (open): "  file)
                      );_if

                      ;; PROPERTIES STRING IF NECESSARY
                      (if propertiesString (write-line propertiesString file))
                      
                      ;; CYCLE THROUGH EACH VERTEXT
                      (while (= "VERTEX" (cdr (assoc 0 (entget (setq ename (entnext ename))))))
                        (setq pt1 (cdr (assoc 10 (entget ename))))
                        (write-line (strcat "  VERTEX:," (rtos (car pt1) 2 2) "," (rtos (cadr pt1) 2 2) "," (rtos (caddr pt1) 2 2))file)
                      );_while

                      (write-line "END OF 2DPOLYLINE" file)

                    );_progn
                  );_(and
                   ;   (= "POLYLINE" (cdr (assoc 0 (entget (ssname ss 0)))))
                   ;   (= 0 (logand (cdr (assoc 70 (entget (ssname ss 0))))8))
                   ; )


                  ;; 3D POLYLINE
                  ((and
                     (= "POLYLINE" (cdr (assoc 0 (entget (ssname ss 0)))))
                     (= 8 (logand (cdr (assoc 70 (entget (ssname ss 0))))8))
                   );_and
                   
                    (progn

                      (setq ename (ssname ss 0))

                      ;; SPECIFY OPENED OR CLOSED LWPOLYLINE
                      (if (= 1 (logand (cdr (assoc 70 (entget ename))) 1))                        
                        (write-line "3DPOLYLINE (closed):" file)
                        (write-line "3DPOLYLINE (open): "  file)
                      );_if

                      ;; PROPERTIES STRING IF NECESSARY
                      (if propertiesString (write-line propertiesString file))
                      
                      ;; CYCLE THROUGH EACH VERTEXT
                      (while (= "VERTEX" (cdr (assoc 0 (entget (setq ename (entnext ename))))))
                        (setq pt1 (cdr (assoc 10 (entget ename))))
                        (write-line (strcat "  VERTEX:," (rtos (car pt1) 2 2) "," (rtos (cadr pt1) 2 2) "," (rtos (caddr pt1) 2 2))file)
                      );_while

                      (write-line "END OF 3DPOLYLINE" file)

                    );_progn
                  );_(and
                   ;   (= "POLYLINE" (cdr (assoc 0 (entget (ssname ss 0)))))
                   ;   (= 8 (logand (cdr (assoc 70 (entget (ssname ss 0))))8))
                   ; )
                  
                );_cond

                ;; REMOVE THE CURRENT ENTITY FROM THE SELECTION SET
                (setq ss (ssdel (ssname ss 0) ss))
                  
              );_while

              ;; CLOSE THE FILE
              (close file)
              (princ (strcat "\nExported to: " filePath))
              (princ)
                  
            );_progn
            (alert (strcat "\nUnable to write to: " filePath))
          );_if

        );_progn
      );_if

    );_progn
    (progn
      (princ "\nNothing Selected")
      (princ)
    );_progn
  );_if
            
);_defun C:LCAD_Export


;; MAIN IMPORT FUNCTION
(defun C:LCAD_Import ( / filePath file lineToProcess pts pt elev elist elistHeader startEnt
                         pt1 pt2 *error*)

  ;; DEFINE ERROR HANDLING
  (defun *error* ( msg / )
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError: " msg "\nABORT: The CSV file is not properly formatted for LCAD_Import"))
    );_if
    (exit)
    (princ)
  );_defun *error*

  ;; DETERMINE THE FILE PATH OF THE FILE TO OPEN
  (if (setq filePath (getfiled "Select a LexmanCAD CSVBackup formatted file" "" "csv" 0))
    (progn
        
      ;; OPEN THE FILE
      (if (setq file (open filePath "r" ))
        (progn

          ;; SKIP THE FIRST LINE (CONTAINING THE HEADERS)
          (read-line file)

          ;; BEGIN READING LINES
          (while (setq lineToProcess (read-line file))

            (cond

              ((vl-string-search "LWPOLYLINE" lineToProcess)

                ;; RECORD WETHER THIS LWPOLYLINE IS CLOSED OR NOT
                (setq code70 (if (vl-string-search "closed" lineToProcess) 1 0))

                ;; CLEAR PTS
                (setq pts nil)

                ;; INITIALIZE PROPERTIES
                (setq layerName nil)
                (setq colourNumber nil)
                (setq lineTypeString nil)
               
                ;; CHECK FOR A PROPERTIES HEADER
                (setq lineToProcess (read-line file))
                (if (vl-string-search "Properties:" lineToProcess)
                  (progn
                    (setq propertiesList (readPropertiesString lineToProcess))
                    (setq layerName (car propertiesList))
                    (setq colourNumber (cadr propertiesList))
                    (setq lineTypeString (caddr propertiesList))
                  );_progn

                  ;; RECORD THE FIRST POINT
                  (setq pts (cons (LCAD_ExtractCoordinates lineToProcess) pts))
                );_if
                  
                ;; RECORD THE REST OF THE POINTS
                (while (vl-string-search "VERTEX" (setq lineToProcess (read-line file)))
                  (setq pts (cons (LCAD_ExtractCoordinates lineToProcess) pts))
                );_while

                (setq pts (reverse pts))
               
                ;; FETCH THE ELEVATION
                (setq elev (caddr (car pts)))

                ;; MAKE THE LINE ENTITY LIST
                (setq elist (list
                              (cons 0 "LWPOLYLINE")
                              (cons 100 "AcDbEntity")
                              (cons 67 0)
                              (cons 8 (if layerName layerName "0"))
                              (cons 100 "AcDbPolyline")
                              (cons 90 (length pts))
                              (cons 70 code70)
                              (cons 38 elev)))

                ;; APPEND COLOUR NUMBER and LINETYPESTRING IF AVAILABLE
                (if colourNumber   (setq elist (append elist (list (cons 62 colourNumber)))))
                (if lineTypeString (setq elist (append elist (list (cons 6 lineTypeString)))))

                ;; ADD THE POINTS TO THE ELIST
                (foreach pt pts
                  (setq elist (append elist (list
                                              (cons 10 (list (car pt) (cadr pt)))
                                              (cons 91 0))))
                );_foreach

                ;; MAKE THE ENTITY
                (entmake elist)
                  
              );_(vl-string-search "LWPOLYLINE" lineToProcess)




              
              ((vl-string-search "3DPOLYLINE" lineToProcess)

                ;; RECORD WHETHER THIS POLYLINE IS CLOSED OR NOT
                (setq code70 (if (vl-string-search "closed" lineToProcess) 1 0))

                ;; CLEAR PTS
                (setq pts nil)

                ;; INITIALIZE PROPERTIES
                (setq layerName nil)
                (setq colourNumber nil)
                (setq lineTypeString nil)
               
                ;; CHECK FOR A PROPERTIES HEADER
                (setq lineToProcess (read-line file))
                (if (vl-string-search "Properties:" lineToProcess)
                  (progn
                    (setq propertiesList (readPropertiesString lineToProcess))
                    (setq layerName (car propertiesList))
                    (setq colourNumber (cadr propertiesList))
                    (setq lineTypeString (caddr propertiesList))
                  );_progn

                  ;; RECORD THE FIRST POINT
                  (setq pts (cons (LCAD_ExtractCoordinates lineToProcess) pts))
                );_if

                ;; MAKE THE 3DPOLYLINE HEADER ENTITY LIST
                (setq elistHeader (list
                                    (cons 0 "POLYLINE")
                                    (cons 100 "AcDbEntity")
                                    (cons 67 0)
                                    (cons 8 (if layerName layerName "0"))
                                    (cons 100 "AcDb3dPolyline")
                                    (cons 10 (list 0.0 0.0 0.0))
                                    (cons 70 (+ 8 code70))
                                    (cons 66 1)))

                ;; APPEND COLOUR NUMBER and LINETYPESTRING IF AVAILABLE
                (if colourNumber   (setq elistHeader (append elistHeader (list (cons 62 colourNumber)))))
                (if lineTypeString (setq elistHeader (append elistHeader (list (cons 6 lineTypeString)))))
               
                ;; RECORD THE POINTS
                (while (vl-string-search "VERTEX" (setq lineToProcess (read-line file)))
                  (setq pts (cons (LCAD_ExtractCoordinates lineToProcess) pts))
                );_while
                (setq pts (reverse pts))
               
                ;; MAKE THE HEADER
                (entmake elistHeader)

                ;; MAKE THE VERTEX ENTITIES
                (foreach pt pts
                  (entmake (list
                             (cons 0 "VERTEX")
                             (cons 100 "AcDbEntity")
                             (cons 67 0)
                             (cons 8 (if layerName layerName "0"))
                             (cons 100 "AcDbVertex")
                             (cons 100 "AcDb3dPolylineVertex")
                             (cons 10 pt)
                             (cons 70 32)))
                );_foreach

                ;; MAKE THE SEQEND
                (entmake (list
                           (cons 0 "SEQEND")
                           (cons 100 "AcDbEntity")
                           (cons 67 0)
                           (cons 8 (if layerName layerName "0"))))

              );_(vl-string-search "3DPOLYLINE" lineToProcess)




              
              ((vl-string-search "2DPOLYLINE" lineToProcess)

                ;; RECORD WHETHER THIS POLYLINE IS CLOSED OR NOT
                (setq code70 (if (vl-string-search "closed" lineToProcess) 1 0))

                ;; CLEAR PTS
                (setq pts nil)

                ;; INITIALIZE PROPERTIES
                (setq layerName nil)
                (setq colourNumber nil)
                (setq lineTypeString nil)
               
                ;; CHECK FOR A PROPERTIES HEADER
                (setq lineToProcess (read-line file))
                (if (vl-string-search "Properties:" lineToProcess)
                  (progn
                    (setq propertiesList (readPropertiesString lineToProcess))
                    (setq layerName (car propertiesList))
                    (setq colourNumber (cadr propertiesList))
                    (setq lineTypeString (caddr propertiesList))
                  );_progn

                  ;; RECORD THE FIRST POINT
                  (setq pts (cons (LCAD_ExtractCoordinates lineToProcess) pts))
                );_if

                ;; RECORD THE POINTS
                (while (vl-string-search "VERTEX" (setq lineToProcess (read-line file)))
                  (setq pts (cons (LCAD_ExtractCoordinates lineToProcess) pts))
                );_while
                (setq pts (reverse pts))
                
                ;; FETCH THE ELEVATION
                (setq elev (caddr (car pts)))

                ;; MAKE THE POLYLINE HEADER ENTITY LIST
                (setq elistHeader (list
                                    (cons 0 "POLYLINE")
                                    (cons 100 "AcDbEntity")
                                    (cons 67 0)
                                    (cons 8 (if layerName layerName "0"))
                                    (cons 100 "AcDb2dPolyline")
                                    (cons 10 (list 0.0 0.0 elev))
                                    (cons 70 code70)
                                    (cons 66 1)))

                ;; APPEND COLOUR NUMBER and LINETYPESTRING IF AVAILABLE
                (if colourNumber   (setq elistHeader (append elistHeader (list (cons 62 colourNumber)))))
                (if lineTypeString (setq elistHeader (append elistHeader (list (cons 6 lineTypeString)))))


                ;; MAKE THE HEADER
                (entmake elistHeader)
              
                ;; MAKE THE VERTEX ENTITIES
                (foreach pt pts
                  (entmake (list
                             (cons 0 "VERTEX")
                             (cons 100 "AcDbEntity")
                             (cons 67 0)
                             (cons 8 (if layerName layerName "0"))
                             (cons 100 "AcDbVertex")
                             (cons 100 "AcDb2dVertex")
                             (cons 10 pt)
                             (cons 91 0)
                             (cons 70 0)))                                                 
                );_foreach

               ;; MAKE THE SEQEND
               (entmake (list
                          (cons 0 "SEQEND")
                          (cons 100 "AcDbEntity")
                          (cons 67 0)
                          (cons 8 (if layerName layerName "0"))))
                  
              );_(vl-string-search "POLYLINE" lineToProcess)

              

              
              
              ((vl-string-search "LINE" lineToProcess)

               ;; INITIALIZE PROPERTIES
               (setq layerName nil)
               (setq colourNumber nil)
               (setq lineTypeString nil)

               (setq pt1 nil)
               (setq pt2 nil)
               (setq elist nil)
               
               ;; CHECK FOR A PROPERTIES HEADER
               (setq lineToProcess (read-line file))
               (if (vl-string-search "Properties:" lineToProcess)
                 (progn
                   (setq propertiesList (readPropertiesString lineToProcess))
                   (setq layerName (car propertiesList))
                   (setq colourNumber (cadr propertiesList))
                   (setq lineTypeString (caddr propertiesList))
                 );_progn

                 ;; RECORD THE FIRST POINT
                 (setq pt1 (LCAD_ExtractCoordinates lineToProcess))
               );_if  

               ;; RECORD BEGINNING AND END POINTS OF THE LINE
               (if pt1
                 (setq pt2 (LCAD_ExtractCoordinates (read-line file)))
                 (progn
                   (setq pt1 (LCAD_ExtractCoordinates (read-line file)))
                   (setq pt2 (LCAD_ExtractCoordinates (read-line file)))
                 );_progn
               );_if

               (setq elist (list
                             (cons 0 "LINE")
                             (cons 100 "AcDbEntity")
                             (cons 67 0)
                             (cons 8 (if layerName layerName "0"))
                             (cons 100 "AcDbLine")
                             (cons 10 pt1)
                             (cons 11 pt2)))
                
               ;; APPEND COLOUR NUMBER and LINETYPESTRING IF AVAILABLE
               (if colourNumber   (setq elist (append elist (list (cons 62 colourNumber)))))
               (if lineTypeString (setq elist (append elist (list (cons 6 lineTypeString)))))

               ;; DRAW THE ENTITY
               (entmake elist)
               
              );_(vl-string-search "LINE" lineToProcess)




              
              ((vl-string-search "POINT" lineToProcess)

               ;; INITIALIZE PROPERTIES
               (setq layerName nil)
               (setq colourNumber nil)
               (setq lineTypeString nil)

               (setq pt nil)
               (setq elist nil)

               ;; CHECK FOR A PROPERTIES HEADER
               (setq lineToProcess (read-line file))
               (if (vl-string-search "Properties:" lineToProcess)
                 (progn
                   (setq propertiesList (readPropertiesString lineToProcess))
                   (setq layerName (car propertiesList))
                   (setq colourNumber (cadr propertiesList))
                   (setq lineTypeString (caddr propertiesList))
                 );_progn

                 ;; RECORD THE POINT
                 (setq pt (LCAD_ExtractCoordinates lineToProcess))
               );_if

               ;; RECORD THE POINT IF NEEDED
               (if (not pt)
                 (setq pt (LCAD_ExtractCoordinates (read-line file)))
               );_if

               ;; BUILD THE ENTITY LIST
               (setq elist (list
                             (cons 0 "POINT")
                             (cons 100 "AcDbEntity")
                             (cons 67 0)
                             (cons 8 "Layer1")
                             (cons 100 "AcDbPoint")
                             (cons 10 pt)))

               ;; APPEND COLOUR NUMBER and LINETYPESTRING IF AVAILABLE
               (if colourNumber   (setq elist (append elist (list (cons 62 colourNumber)))))
               (if lineTypeString (setq elist (append elist (list (cons 6 lineTypeString)))))
               
               ;; DRAW THE ENTITY
               (entmake elist)
                  
              );_(vl-string-search "POINT" lineToProcess)            


              
            );_cond

          );_while

          (close file)
          (princ (strcat "\nImported from: " filePath))
          (princ)          
        );_progn
        (alert (strcat "Unable to open: " filePath))
      );_if

    );_progn
    (progn
      (princ "\nNo file selected")
      (princ)
    );_progn
  );_if


);_defun C:LCAD_Import


;; OTHER FUNCTIONS

(defun LCAD_ExtractCoordinates ( string / commaPosition xString x Ystring y z )
;; STRINGS WILL LOOK LIKE: "  VERTEX:,-1.17,-0.09,0"

  ;; CUT EVERYTHING BEFORE THE FIRST COMMA (INCLUDING THE FIRST COMMA)
  (setq commaPosition (vl-string-search "," string))
  (setq string (substr string (+ commaPosition 2)))
    
  ;; RECORD THE SECOND COMMA POSITION
  (setq commaPosition (vl-string-search "," string))
  ;; RECORD THE FIRST POINT STRING
  (setq xString (substr string 1 commaPosition))
  ;; RECORD THE FIRST POINT
  (setq x (atof xString))

  ;; CUT EVERYTHING BEFORE THE SECOND COMMA
  (setq string (substr string (+ commaPosition 2)))
  ;; RECORD THE THIRD COMMA POSITION
  (setq commaPosition (vl-string-search "," string))
  ;; RECORD THE SECOND POINT STRING
  (setq yString (substr string 1 commaPosition))
  ;; RECORD THE SECOND POINT
  (setq y (atof yString))

  ;; CUT EVERYTHING BEFORE THE THIRD COMMA
  (setq string (substr string (+ commaPosition 2)))
  ;; RECORD THE THIRD POINT
  (setq z (atof string))

  ;; RETURN PT1 PT2 PT3
  (list x y z)

);_defun LCAD_ExtractCoordinates


;; BUILD PROPERTIES STRING
(defun buildPropertiesString ( entName / entString propertiesString layerString colourString lineTypeString)

  (setq entString (entget entName))
    
  ;; FETCH THE LAYER STRING
  (if (/= (cdr (assoc 8 entString)) "0")
    (setq layerString (cdr (assoc 8 entString)))
    (setq layerString nil)
  );_if

  ;; FETCH THE COLOUR STRING
  (if (and (assoc 62 entString) (/= (cdr (assoc 62 entString)) 0))
    (setq colourString (itoa (cdr (assoc 62 entString))))
    (setq colourString nil)
  );_if

  ;; FETCH THE LINETYPE STRING
  (if (assoc 6 entString)
    (setq lineTypeString (cdr (assoc 6 entString)))
    (setq lineTypeString nil)
  );_if

  ;; RETURN THE PROPERTIES STRING OR NIL
  (if (or layerString colourString lineTypeString)
    (setq propertiesString (strcat
                             "Properties:,"
                             (if layerString    (strcat "Layer:,"    layerString    ",") "")
                             (if colourString   (strcat "Colour:,"   colourString   ",") "")
                             (if lineTypeString (strcat "lineType:," lineTypeString ",") "")))
    nil
  );_if

);_defun buildPropertiesString


;; READ PROPERTIES STRING
;; RETURNS A LIST WHICH CORRESPONDS TO THE ENTRIES FOR CODES 8 62 6 WHICH ARE LAYER/COLOUR/LINETYPE
(defun readPropertiesString ( string / cursor endPos returnList layerString colourNumber lineTypeString subString)

  ;; CUT EVERYTHING BEFORE THE FIRST COMMA (INCLUDING THE FIRST COMMA)
  (setq cursor (vl-string-search "," string))
  (setq string (substr string (+ cursor 2)))

  ;; FETCH THE LAYER STRING
  (if (vl-string-search "Layer:" string)
    (progn
      (setq cursor (vl-string-search "Layer:," string))
      (setq cursor (vl-string-search "," string cursor))
      (setq endPos (vl-string-search "," string (+ cursor 1)))
      (setq layerString (substr string (+ cursor 2) (if endPos (- endPos cursor 1))))
    );_progn
    (setq layerString nil)
  );_if

  ;; FETCH THE COLOUR NUMBER
  (if (vl-string-search "Colour:" string)
    (progn
      (setq cursor (vl-string-search "Colour:," string))
      (setq cursor (vl-string-search "," string cursor))
      (setq endPos (vl-string-search "," string (+ cursor 1)))
      (setq colourNumber (atoi (substr string (+ cursor 2) (if endPos (- endPos cursor 1)))))
    );_progn
    (setq colourNumber nil)
  );_if

  ;; FETCH THE lineTypeString
  (if (vl-string-search "lineType:" string)
    (progn
      (setq cursor (vl-string-search "lineType:," string))
      (setq cursor (vl-string-search "," string cursor))
      (setq endPos (vl-string-search "," string (+ cursor 1)))
      (setq lineTypeString (substr string (+ cursor 2) (if endPos (- endPos cursor 1))))
    );_progn
    (setq lineTypeString nil)
  );_if

  ;; RETURN THE LIST
  (setq returnList (list layerString colourNumber lineTypeString))

);_defun readPropertiesString