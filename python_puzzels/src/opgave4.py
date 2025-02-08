    def __add__(self, other):
        """ Returns a valid Time objects which is Time objects
        'other' added to 'self'.
        >>> print(Time(0,0,0) + Time(1,2,3))
        01:02:03
        >>> print(Time(13,30,0) + Time(1,46,-45))
        15:15:15
        """

    def __sub__(self, other):
        """ Returns a valid Time objects which is Time objects
        'other' substracted from 'self'.
        >>> print(Time(10,10,10) - Time(1,2,3))
        09:08:07
        >>> print(Time(10,0,0) - Time(1,50,600))
        08:00:00
        """
