import numpy as np
from scipy import stats
import math

def times(mode,task,max_k):
    all_times = []
    for k in range(1,max_k+1):
        k_results = []
        with open('programs/{}-{}-{}.pl'.format(mode,task,k)) as f:
            for line in f:
                xs = line.strip().split(',')
                if xs[0]!='%time':
                  continue
                k_results.append(float(xs[1]))
        all_times.append(np.mean(k_results))
    return g(np.mean(all_times)),g(stats.sem(all_times))

def g(x):
    if x > 1:
        return int(x)
    return round(x,2)

def accuracies(mode,task,max_k):
    all_accuracies = []
    for k in range(1,max_k+1):
        k_results = []
        with open('results/{}-{}-{}.txt'.format(mode,task,k)) as f:
            for line in f:
                try:
                    x = float(line.strip())
                    k_results.append(x)
                    # all_accuracies.append(x)
                except:
                    pass
        all_accuracies.append(np.mean(k_results))
    return r(np.mean(all_accuracies)),r(stats.sem(all_accuracies))

def r(x):
    # return round(x*100,2)
    return int(round(x*100))

def tmp_times(mode,task,max_k):
    all_times = []
    for k in range(1,max_k+1):
        with open('programs/{}-{}-{}.pl'.format(mode,task,k)) as f:
            for line in f:
                xs = line.strip().split(',')
                if xs[0]!='%time':
                  continue
                all_times.append(float(xs[1]))
    return all_times

def tmp_accs(mode,task,max_k):
    all_results = []
    for k in range(1,max_k+1):
        with open('results/{}-{}-{}.txt'.format(mode,task,k)) as f:
            for line in f:
                try:
                    x = int(line.strip())
                    all_results.append(x)
                except:
                    pass
    return all_results

max_k=20
print 'ACCURACIES'
for task in range(1,9):
    (e_xs,e_err) = accuracies('e',task,max_k)
    (d_xs,d_err) = accuracies('d',task,max_k)
    (d2_xs,d2_err) = accuracies('d2',task,max_k)
    print 'T{} & {} $\pm$ {} & {} $\pm$ {} & {} $\pm$ {}\\\\'.format(task,e_xs,e_err,d_xs,d_err,d2_xs,d2_err)

print 'TIMES'
for task in range(1,9):
    (e_xs,e_err) = times('e',task,max_k)
    (d_xs,d_err) = times('d',task,max_k)
    (d2_xs,d2_err) = times('d2',task,max_k)
    print 'T{} & {} $\pm$ {} & {} $\pm$ {} & {} $\pm$ {}\\\\'.format(task,e_xs,e_err,d_xs,d_err,d2_xs,d2_err)

# # task=8
# max_k=20
# # all_e = []
# # all_d = []
# # all_d2 = []

# # print 'ACCURACIES'
# # for task in range(1,9):
# #     (e_xs,e_err) = accuracies('e',task,max_k)
# #     (d_xs,d_err) = accuracies('d',task,max_k)
# #     (d2_xs,d2_err) = accuracies('d2',task,max_k)
# #     print 'T{} & {} $\pm$ {} & {} $\pm$ {} & {} $\pm$ {}\\\\'.format(task,e_xs,e_err,d_xs,d_err,d2_xs,d2_err)
#     # all_e.extend(tmp_accs('e',task,max_k))
#     # all_d.extend(tmp_accs('d',task,max_k))
#     # all_d2.extend(tmp_accs('d2',task,max_k))

# # print all_d[:5]
# # print all_d2[:5]
# b = sum(1.0 for (x,y) in zip(all_d,all_d2) if x == 1 and y == 0)
# c = sum(1.0 for (x,y) in zip(all_d,all_d2) if x == 0 and y == 1)
# # print b,c
# # McN = math.pow((b-c),2) / (b+c)
# # print 'P-value: %f'%(1-stats.chi2.cdf(McN,1))

# # all_e = []
# # all_d = []
# # all_d2 = []

# # print 'TIMES'
# # for task in range(1,8):
# #     all_e.extend(tmp_times('e',task,max_k))
# #     all_d.extend(tmp_times('d',task,max_k))
# #     all_d2.extend(tmp_times('d2',task,max_k))

# # print stats.ttest_rel(all_d,all_d2)



