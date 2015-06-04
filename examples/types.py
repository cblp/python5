items = iter([{'is_good': True}])

#-------------------------------------------------------------------------------

def create_error():
    return ValueError()

def get_next_item():
    return next(items, None)

def check_next_item():
    good = get_next_item()['is_good']
    if not good:
        create_error()

#-------------------------------------------------------------------------------

check_next_item()
check_next_item()
