-- Why does guiFetch combine worker functions instead of calling statusWindow
-- twice?


-- The most important reason is: Considering how the operations are implemented,
-- updating the podcast and downloading its episodes need to be run sequentially
-- in that order because the downloading can produce expected results only when
-- it knows the latest information about the podcast's episodes.
--
-- Calling statusWindow twice would run those operations concurrently and their
-- execution ordering could not be predicted. For example, the downloading could
-- be run before the updating.
--
-- Other than that, the statusWindow uses the same GUI status window. The latter
-- call to it would overwrite information from the former call, replacing the
-- callback for canceling the child thread. Then it wouldn't be possible for the
-- user to cancel the first thread.
