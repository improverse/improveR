# This file previously contained autoRefresh functions that have been replaced
# by the shared token management system in sharedTokenManager.R
# 
# The old autoRefresh functions (autoRefreshStart, autoRefreshRunning, etc.)
# have been superseded by:
# - startSharedTokenRefresh() 
# - isSharedTokenRefreshRunning()
# - readSharedRefreshedTokens()
# - stopSharedTokenRefresh()
# 
# These new functions provide better multi-session token sharing with the same
# XOR encryption security as the original implementation.