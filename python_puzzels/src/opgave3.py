    def get_total_seconds(self):
        """ Returns the number of seconds since time 00:00:00.
        >>> Time(0,0,1).get_total_seconds()
        1
        >>> Time(0,1,0).get_total_seconds()
        60
        >>> Time(1,0,0).get_total_seconds()
        3600
        >>> Time(13,30,5).get_total_seconds()
        48605
        """
