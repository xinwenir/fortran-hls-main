"""
***************************************************************
    Copyright 2023 Hewlett Packard Enterprise Development LP.
***************************************************************
"""

class NoneFilename(Exception):
    """
    Exception raised when there are not files defined for operation in the processor class.
    """

    def __init__(self):
        self.message = "Input or output filename is None"
        super().__init__(self.message)
