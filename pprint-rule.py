# metarule([P,Q,X,Y],([P,X,A]:-[[Q,Y,A]])).

def pformat(rule):
    # print rule
    next_letter_ord = ord('P')
    next_letter = chr(next_letter_ord)
    head = [next_letter] + rule[0]
    subs = [next_letter]
    body = []
    for literal in rule[1:]:
        next_letter_ord+=1
        next_letter = chr(next_letter_ord)
        body += [[next_letter] + literal]
        subs += [next_letter]
        # out +=','

    # out = out[:-1] + '.'

    out = 'metarule({},({} :- {})).'.format(subs,head,body)
    out = out.replace("'","")
    return out

# def pliteral(next_letter,vars):
    # return


# e-reduction
xs = [
[['A'],['A']],
[['A','B'],['B','A']],
[['A','B'],['A'],['B']],
[['A','B'],['A','C'],['C','B']],
[['A'],['A','B'],['A','B']]
]

# d-reduction
# xs = [
# [['A'],['A']],
# [['A','B'],['B','A']],
# [['A'],['A'],['A']],
# [['A','B'],['A'],['B']],
# [['A','B'],['A'],['A','B']],
# [['A'],['B'],['B','A']],
# [['A','B'],['A','B'],['A','B']],
# [['A','B'],['A','C'],['B','C']],
# [['A'],['B','A'],['B','A']],
# [['A'],['B','A'],['B','C'],['B','C']],
# [['A','B'],['A','B'],['A','C'],['C','D'],['C','D']],
# [['A','B'],['A','C'],['A','D'],['B','C'],['B','D'],['C','D']]
# ]

for x in xs:
    print pformat(x)