import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

class PostPredBivariate:
    def __init__(self, posterior_az, Y180, Y120, test_train=False, idx_test=None):
        self.Y180_test = None
        self.Y120_test = None
        self.posterior_az = posterior_az
        self.posterior = posterior_az.posterior
        self.posterior_med = self.posterior.median(dim=['chain', 'draw'])

        # Indici per Count_180 e Count_120
        self.idx_obs180 = Y180[Y180.notna()].index
        self.idx_miss180 = Y180[Y180.isna()].index
        self.idx_obs120 = Y120[Y120.notna()].index
        self.idx_miss120 = Y120[Y120.isna()].index

        if test_train:
            self.idx_test = idx_test
            self.Y180_test = Y180[idx_test].reset_index(drop=True)
            self.Y120_test = Y120[idx_test].reset_index(drop=True)

            # Indici osservati e mancanti per training
            self.idx_obs180 = sorted(np.setdiff1d(self.idx_obs180, self.idx_test))
            self.idx_miss180 = sorted(np.concatenate([self.idx_miss180, self.idx_test]))
            self.idx_obs120 = sorted(np.setdiff1d(self.idx_obs120, self.idx_test))
            self.idx_miss120 = sorted(np.concatenate([self.idx_miss120, self.idx_test]))

        self.Y180_miss = Y180[self.idx_miss180].reset_index(drop=True)
        self.Y180_obs = Y180[self.idx_obs180].reset_index(drop=True)
        self.Y120_miss = Y120[self.idx_miss120].reset_index(drop=True)
        self.Y120_obs = Y120[self.idx_obs120].reset_index(drop=True)

        self.Y180 = np.array(Y180)
        self.Y120 = np.array(Y120)

        if 'y_pred_miss180' in self.posterior and 'y_pred_miss120' in self.posterior:
            self.Y180[Y180.isna()] = np.nan
            self.Y120[Y120.isna()] = np.nan
        else:
            self.Y180 = self.Y180[self.idx_obs180]
            self.Y120 = self.Y120[self.idx_obs120]

    def predict(self, use_mean=False, CI=False, alpha=0.05, error_metrics=False):
        if CI:
            quantiles = [0.5, alpha/2, 1-alpha/2]
        else:
            quantiles = [0.5]


        # Predizioni per Count_180
        if use_mean:
            y_pred_obs180 = self.posterior.y_pred180.mean(dim=['chain', 'draw']).T
            y_pred_obs180 = pd.DataFrame(y_pred_obs180.values, columns=['pred180'], index = self.idx_obs180)
            if CI:
                y_pred_obs_CI180 = self.posterior.y_pred180.quantile(quantiles[1, 2], dim=['chain', 'draw']).T
                y_pred_obs_CI180 = pd.DataFrame(y_pred_obs_CI180.values, columns=[f'{alpha/2}', f'{1-alpha/2}'], index = self.idx_obs180)
                y_pred_obs180 = pd.concat([y_pred_obs180, y_pred_obs_CI180], axis=1)
        else:
            y_pred_obs180 = self.posterior.y_pred180.quantile(quantiles, dim=['chain', 'draw']).T
            y_pred_obs180 = pd.DataFrame(y_pred_obs180.values, index = self.idx_obs180)
            if CI:
                y_pred_obs180.columns = ['pred180', f'{alpha/2}', f'{1-alpha/2}']
            else:
                y_pred_obs180.columns = ['pred180']

        # Predizioni per Count_120
        if use_mean:
            y_pred_obs120 = self.posterior.y_pred120.mean(dim=['chain', 'draw']).T
            y_pred_obs120 = pd.DataFrame(y_pred_obs120.values, columns=['pred120'], index = self.idx_obs120)
            if CI:
                y_pred_obs_CI120 = self.posterior.y_pred120.quantile(quantiles[1, 2], dim=['chain', 'draw']).T
                y_pred_obs_CI120 = pd.DataFrame(y_pred_obs_CI120.values, columns=[f'{alpha/2}', f'{1-alpha/2}'], index = self.idx_obs120)
                y_pred_obs120 = pd.concat([y_pred_obs120, y_pred_obs_CI120], axis=1)
        else:
            y_pred_obs120 = self.posterior.y_pred120.quantile(quantiles, dim=['chain', 'draw']).T
            y_pred_obs120 = pd.DataFrame(y_pred_obs120.values, index = self.idx_obs120)
            if CI:
                y_pred_obs120.columns = ['pred120', f'{alpha/2}', f'{1-alpha/2}']
            else:
                y_pred_obs120.columns = ['pred120']


        # Gestione dei valori mancanti
        if 'y_pred_miss180' in self.posterior and 'y_pred_miss120' in self.posterior:
            if use_mean:
                y_pred_miss180 = self.posterior.y_pred_miss180.mean(dim=['chain', 'draw']).T
                y_pred_miss120 = self.posterior.y_pred_miss120.mean(dim=['chain', 'draw']).T

                y_pred_miss180 = pd.DataFrame(y_pred_miss180.values, columns=['pred180'], index=self.idx_miss180)
                y_pred_miss120 = pd.DataFrame(y_pred_miss120.values, columns=['pred120'], index=self.idx_miss120)
                if CI:
                    y_pred_miss_CI180 = self.posterior.y_pred_miss180.quantile(quantiles[1, 2], dim=['chain', 'draw']).T
                    y_pred_miss_CI120 = self.posterior.y_pred_miss120.quantile(quantiles[1, 2], dim=['chain', 'draw']).T

                    y_pred_miss_CI180 = pd.DataFrame(y_pred_obs_CI180.values, columns=[f'{alpha/2}', f'{1-alpha/2}'], index = self.idx_miss180)
                    y_pred_miss_CI120 = pd.DataFrame(y_pred_obs_CI120.values, columns=[f'{alpha/2}', f'{1-alpha/2}'], index = self.idx_miss120)

                    y_pred_miss180 = pd.concat([y_pred_miss180, y_pred_miss_CI180], axis=1)
                    y_pred_miss120 = pd.concat([y_pred_miss120, y_pred_miss_CI120], axis=1)

            else:
                y_pred_miss180 = self.posterior.y_pred_miss180.quantile(quantiles, dim=['chain', 'draw']).T
                y_pred_miss120 = self.posterior.y_pred_miss120.quantile(quantiles, dim=['chain', 'draw']).T

                y_pred_miss180 = pd.DataFrame(y_pred_miss180.values, index = self.idx_miss180)
                y_pred_miss120 = pd.DataFrame(y_pred_miss120.values, index = self.idx_miss120)
                if CI:
                    y_pred_miss180.columns = ['pred180', f'{alpha/2}', f'{1-alpha/2}']
                    y_pred_miss120.columns = ['pred120', f'{alpha/2}', f'{1-alpha/2}']
                else:
                    y_pred_miss180.columns = ['pred180']
                    y_pred_miss120.columns = ['pred120']

            y_pred180 = pd.concat([y_pred_obs180, y_pred_miss180], axis=0).sort_index()
            y_pred120 = pd.concat([y_pred_obs120, y_pred_miss120], axis=0).sort_index()

        else:
            y_pred180 = y_pred_obs180.reset_index(drop=True)
            y_pred120 = y_pred_obs120.reset_index(drop=True)

        # Metriche di errore (applicate separatamente per Count_180 e Count_120)
        if error_metrics:
            if self.Y180_test is not None and 'y_pred_miss180' in self.posterior:
                idx_obs_test180 = sorted(np.concatenate([self.idx_obs180, self.idx_test]))
                y_star180 = y_pred180['pred180'][idx_obs_test180].reset_index(drop=True)
                residuals180 = y_star180 - self.Y180[idx_obs_test180]
                y_star_test180 = y_pred180['pred180'][self.idx_test].reset_index(drop=True)
                residuals_test180 = y_star_test180 - self.Y180[self.idx_test]
            else:
                y_star180 = y_pred_obs180['pred180'].reset_index(drop=True)
                residuals180 = y_star180 - self.Y180_obs

            mse180 = np.mean(residuals180**2)
            mae180 = np.mean(np.abs(residuals180))
            mad180 = np.median(np.abs(residuals180))
            rmse180 = np.sqrt(mse180)

            metrics180 = {
                'y_obs180': y_star180,
                'residuals180': residuals180,
                'mse180': mse180,
                'mae180': mae180,
                'mad180': mad180,
                'rmse180': rmse180,
            }

            if self.Y180_test is not None and 'y_pred_miss180' in self.posterior:
                mse_test180 = np.mean(residuals_test180**2)
                mae_test180 = np.mean(np.abs(residuals_test180))
                mad_test180 = np.median(np.abs(residuals_test180))
                rmse_test180 = np.sqrt(mse_test180)
               
                metrics180['mse_test180'] = mse_test180
                metrics180['mae_test180'] = mae_test180
                metrics180['mad_test180'] = mad_test180
                metrics180['rmse_test180'] = rmse_test180

            # Ripetere per Count_120
            if self.Y120_test is not None and 'y_pred_miss120' in self.posterior:
                idx_obs_test120 = sorted(np.concatenate([self.idx_obs120, self.idx_test]))
                y_star120 = y_pred120['pred120'][idx_obs_test120].reset_index(drop=True)
                residuals120 = y_star120 - self.Y120[idx_obs_test120]
                y_star_test120 = y_pred120['pred120'][self.idx_test].reset_index(drop=True)
                residuals_test120 = y_star_test120 - self.Y120[self.idx_test]
            else:
                y_star120 = y_pred_obs120['pred120'].reset_index(drop=True)
                residuals120 = y_star120 - self.Y120_obs

            mse120 = np.mean(residuals120**2)
            mae120 = np.mean(np.abs(residuals120))
            mad120 = np.median(np.abs(residuals120))
            rmse120 = np.sqrt(mse120)

            metrics120 = {
                'y_obs120': y_star120,
                'residuals120': residuals120,
                'mse120': mse120,
                'mae120': mae120,
                'mad120': mad120,
                'rmse120': rmse120,
            }

            if self.Y120_test is not None and 'y_pred_miss120' in self.posterior:
                mse_test120 = np.mean(residuals_test120**2)
                mae_test120 = np.mean(np.abs(residuals_test120))
                mad_test120 = np.median(np.abs(residuals_test120))
                rmse_test120 = np.sqrt(mse_test120)
               
                metrics120['mse_test120'] = mse_test120
                metrics120['mae_test120'] = mae_test120
                metrics120['mad_test120'] = mad_test120
                metrics120['rmse_test120'] = rmse_test120
            
            if CI:
                if self.Y180_test is not None and 'y_pred_miss180' in self.posterior:
                    idx_obs_test180 = sorted(np.concatenate([self.idx_obs180, self.idx_test]))
                    y_star_up180 = y_pred180[f'{1-alpha/2}'][idx_obs_test180].reset_index(drop=True)
                    y_star_low180 = y_pred180[f'{alpha/2}'][idx_obs_test180].reset_index(drop=True)
                    outliers180 = np.where((self.Y180[idx_obs_test180] > y_star_up180) | (self.Y180[idx_obs_test180] < y_star_low180))[0]
                    y_star_up_test180 = y_pred180[f'{1-alpha/2}'][self.idx_test].reset_index(drop=True)
                    y_star_low_test180 = y_pred180[f'{alpha/2}'][self.idx_test].reset_index(drop=True)
                    outliers_test180 = np.where((self.Y180[self.idx_test] > y_star_up_test180) | (self.Y180[self.idx_test] < y_star_low_test180))[0]
                    metrics180['outliers_test180'] = outliers_test180
                    percentage_inside180 = 1 - len(outliers180)/len(self.Y180[idx_obs_test180])
                    percentage_inside_test180 = 1 - len(outliers_test180)/len(self.Y180[self.idx_test])
                    metrics180['percentage_inside_CI_test180'] = percentage_inside_test180
                else:
                    y_star_up180 = y_pred_obs180[f'{1-alpha/2}'].reset_index(drop=True)
                    y_star_low180 = y_pred_obs180[f'{alpha/2}'].reset_index(drop=True)
                    outliers180 = np.where((self.Y180_obs > y_star_up180) | (self.Y180_obs < y_star_low180))[0]
                    percentage_inside180 = 1 - len(outliers180)/len(self.Y180_obs)
                metrics180['outliers180'] = outliers180
                metrics180['percentage_inside_CI180'] = percentage_inside180

                # Ripetere per Count_120
                if self.Y120_test is not None and 'y_pred_miss120' in self.posterior:
                    idx_obs_test120 = sorted(np.concatenate([self.idx_obs120, self.idx_test]))
                    y_star_up120 = y_pred120[f'{1-alpha/2}'][idx_obs_test120].reset_index(drop=True)
                    y_star_low120 = y_pred120[f'{alpha/2}'][idx_obs_test120].reset_index(drop=True)
                    outliers120 = np.where((self.Y120[idx_obs_test120] > y_star_up120) | (self.Y120[idx_obs_test120] < y_star_low120))[0]
                    y_star_up_test120 = y_pred120[f'{1-alpha/2}'][self.idx_test].reset_index(drop=True)
                    y_star_low_test120 = y_pred120[f'{alpha/2}'][self.idx_test].reset_index(drop=True)
                    outliers_test120 = np.where((self.Y120[self.idx_test] > y_star_up_test120) | (self.Y120[self.idx_test] < y_star_low_test120))[0]
                    metrics120['outliers_test120'] = outliers_test120
                    percentage_inside120 = 1 - len(outliers120)/len(self.Y120[idx_obs_test120])
                    percentage_inside_test120 = 1 - len(outliers_test120)/len(self.Y120[self.idx_test])
                    metrics120['percentage_inside_CI_test120'] = percentage_inside_test120
                else:
                    y_star_up120 = y_pred_obs120[f'{1-alpha/2}'].reset_index(drop=True)
                    y_star_low120 = y_pred_obs120[f'{alpha/2}'].reset_index(drop=True)
                    outliers120 = np.where((self.Y120_obs > y_star_up120) | (self.Y120_obs < y_star_low120))[0]
                    percentage_inside120 = 1 - len(outliers120)/len(self.Y120_obs)
                metrics120['outliers120'] = outliers120
                metrics120['percentage_inside_CI120'] = percentage_inside120

            return (y_pred180, y_pred120), (metrics180, metrics120)

        return y_pred180, y_pred120
