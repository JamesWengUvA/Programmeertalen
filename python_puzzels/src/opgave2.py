    def set_time(self, hours, minutes, seconds):
        """ Sets the time of the Time object to 'hours', 'minutes',
        and 'seconds' making sure the values are in valid range:
          hours:   [0, HOURS_IN_DAY)
          minutes: [0, MINUTES_IN_HOUR)
          seconds: [0, SECONDS_IN_MINUTE)
        >>> time = Time(0, 0, 0)
        >>> time.set_time(0, 0, 90)
        >>> print(time)
        00:01:30
        >>> time.set_time(0, 0, 3600)
        >>> print(time)
        01:00:00
        >>> time.set_time(0, 0, -1)
        >>> print(time)
        23:59:59
        >>> time.set_time(10, -121, 0)
        >>> print(time)
        07:59:00
        >>> time.set_time(-50, 0, 0)
        >>> print(time)
        22:00:00
        >>> print(Time(10, -120, -150)) # __init__() test
        07:57:30
        """
